
use crate::ExternalFunction;
use crate::FunctionPointer;
use crate::QuFnObject;
use crate::QuOp;
use crate::QuOp::*;
use crate::QuParser;
use crate::QuRegisterStruct;
use crate::QuStackId;
use crate::QuVoid;
use crate::import::QuRegistered;
use crate::import::QuStruct;
use crate::import::ClassId;
use crate::parser;
use crate::parser::FLOW_TYPE_IF;
use crate::parser::FLOW_TYPE_WHILE;
use crate::parser::KEYWORD_IF;
use crate::parser::QuOperator;
use crate::parser::QuParamNode;
use crate::parser::parsed::*;

use crate::QuMsg;
use crate::QuToken;
use crate::vm::BASE_MODULE;
use crate::vm::QuConstId;
use crate::vm::Stack;

use core::panic;
use std::any::Any;
use std::collections::HashMap;
use std::default;
use std::mem::size_of;
use std::mem::take;
use std::hash::Hash;

// TODO: Fix compiler's documentation

/// Acts like a Python 'with' statement. Takes an opening and closing function,
/// calling the code between them.
macro_rules! with {
	($($start:ident).+, $($end:ident).+ $code:block) => {
		{
			$($start).+();
			let mut lmda = ||{$code};
			let output = lmda();
			$($end).+();
			output
		}
	};
}

#[derive(Debug, Default, Clone)]
pub struct AllocatingStack {
	allocated: usize,
	data: Vec<u8>,
} impl AllocatingStack {

	fn allocate(&mut self, size: usize) -> usize {
		let index = self.allocated;
		self.allocated += size;
		if self.allocated > self.data.len() {
			self.data.resize(self.allocated, 0);
		}
		index
	}

} impl Stack for AllocatingStack {
	type Indexer = usize;


	fn data(&self) -> &Vec<u8> {
		&self.data
	}

	fn data_mut(&mut self) -> &mut Vec<u8> {
		&mut self.data
	}

	fn offset(&self) -> usize {0}

	fn offset_mut(&mut self) -> &mut usize {todo!()}
}


#[derive(Debug, Clone)]
pub struct ClassMetadata {
	constants_map: HashMap<String, ConstantId>,
	external_functions_map: HashMap<FunctionIdentity, ExternalFunctionId>,
	functions_map: HashMap<FunctionIdentity, ConstantId>,

	/// The name of this class.
	name: String,
	/// The ID of this class.
	id: usize,
} impl Default for ClassMetadata {
	fn default() -> Self {
		Self {
			name: "void".to_owned(),
			..Default::default()
		}
	}
}


pub type ConstantId = usize;


pub struct ModuleBuilder<'a>(&'a mut Definitions, ModuleId);
impl<'a> ModuleBuilder<'a> {
	pub fn register_struct<S:QuRegisterStruct+'static>(
		&mut self
	) -> Result<(), QuMsg> {
		self.0.register_module_struct::<S>(self.1)
	}
}


#[derive(Debug, Default, Clone)]
pub struct Definitions {
	pub constants: AllocatingStack,
	pub classes: Vec<QuStruct>,
	pub external_functions: Vec<ExternalFunction>,
	pub functions: Vec<FunctionMetadata>,
	pub modules: Vec<ModuleMetadata>,

	/// A map of names to module IDs.
	pub module_map: HashMap<String, ModuleId>,
	pub imports: QuRegistered,
	/// All registered classes with external functions that were not registered.
	with_unregistered_functions: Vec<ClassId>,
} impl Definitions {
	pub fn new(imports: QuRegistered) -> Self {
		Self {
			imports,
			..Default::default()
		}
	}


	pub fn class_id<T:QuRegisterStruct>(
		&self
	) -> Result<ClassId, QuMsg> {
		self.find_class_id(<T as QuRegisterStruct>::name())
	}


	pub fn define_class_constant<T>(
		&mut self,
		module: ModuleId,
		class: &mut ClassMetadata,
		name: String,
		value: T,
	) -> ConstantId {
		let constant_id = self.define_constant(module, value);
		class.constants_map.insert(name, constant_id);
		constant_id
	}


	pub fn define_constant<T>(
		&mut self,
		module: ModuleId,
		value: T,
	) -> ConstantId {
		let constant_id = self.constants.allocate(size_of::<T>());
		self.constants.write(constant_id, value);
		constant_id
	}


	pub fn define_function(
		&mut self,
		identity: FunctionIdentity,
	) -> Result<FunctionId, QuMsg> {
		let funciton_id = self.functions.len();

		// Update module's function map
		let first_arg_id = match identity.parameters.first() {
			Some(id) => *id,
			None => self.class_id::<QuVoid>()?,
		};
		let first_arg_class = self.get_class_mut(first_arg_id)?;
		if first_arg_class.functions_map.contains_key(&identity) {
			// ERROR: a function with a similar signature was already defined.
			let some_function_id = self.get_some_function_id_by_identity(&identity)?;
			let conflicting_identity = match some_function_id {
				SomeFunctionId::Qu(id) => {
					self.get_function(id).unwrap().identity.clone()
				},
				SomeFunctionId::External(id) => {
					let external_function = self
						.get_external_function(id)
						.unwrap();
					let mut parameters = vec![];
					for parameter in &external_function.parameters {
						parameters.push(
							self.find_class_id(parameter).unwrap()
						);
					}
					FunctionIdentity {
						name: external_function.name.clone(),
						parameters,
						return_type: self.find_class_id(
							external_function.return_type
						).unwrap(),
					}
				},
			};
			return Err(format!(
				"Can't define function with identity '{:?}' because a function with a similar identity was already defined {:?}",
				&identity,
				&conflicting_identity,
			).into());
		}
		first_arg_class.functions_map.insert(identity.clone(), funciton_id);

		// Push function
		self.functions.push(FunctionMetadata {
			identity,
			..Default::default()
		});

		Ok(funciton_id)
	}


	pub fn define_module(
		&mut self,
		name: String,
		body: &dyn Fn(ModuleBuilder) -> Result<(), QuMsg>,
	) -> Result<ModuleId, QuMsg> {
		assert!(!self.module_map.contains_key(&name));
		let id = self.modules.len();
		self.modules.push(ModuleMetadata {
			name: name.clone(),
			id,
			..Default::default()
		});
		self.module_map.insert(name, id);
		(body)(ModuleBuilder(self, id))?;
		Ok(id)
	}


	pub fn define_module_class(
		&mut self, module_id: ModuleId, name: String,
	) -> Result<ClassId, QuMsg> {
		let id = ClassId::from(self.classes.len());
		self.classes.push(QuStruct {
			name: name.clone(),
			..Default::default()
		});
		let module = self.get_module_mut(module_id)?;
		assert!(!module.class_map.contains_key(&name));
		module.class_map.insert(name, id);
		
		Ok(id)
	}


	pub fn define_module_constant<T>(
		&mut self,
		module: ModuleId,
		name: String,
		value: T,
	) -> ConstantId {
		let constant_id = self.define_constant(module, value);
		self.modules[module].constants_map.insert(name, constant_id);
		constant_id
	}


	pub fn get_class(&self, id: ClassId) -> Result<&QuStruct, QuMsg> {
		let class = self.classes
			.get(usize::from(id))
			.ok_or_else(|| -> QuMsg { format!(
				"There's no class with id '{:?}' defined.", id,
			).into()})?;
		Ok(class)
	}


	fn get_class_mut(
		&mut self, id: ClassId,
	) -> Result<&mut QuStruct, QuMsg> {
		let class = self.classes
			.get_mut(usize::from(id))
			.ok_or_else(|| -> QuMsg { format!(
				"There's no class with id '{:?}' defined.", id,
			).into()})?;
		Ok(class)
	}


	fn get_class_constant_id(
		&self, class_id: ClassId, name: &str,
	) -> Result<ConstantId, QuMsg> {
		let class = self.get_class(class_id)?;
		let constant_id = *class.constants_map
			.get(name)
			.ok_or_else(|| -> QuMsg { format!(
				"The class '{}' has no constant named '{}' defined.",
				class.name,
				name,
			).into()})?;
		Ok(constant_id)
	}


	fn get_constant<T>(&self, id: usize) -> &T {
		self.constants.read(id)
	}


	pub fn get_external_function(
		&self, id: ExternalFunctionId
	) -> Result<&ExternalFunction, QuMsg> {
		let external_function = self.external_functions
			.get(usize::from(id))
			.ok_or_else(|| -> QuMsg { format!(
				"There's no external function with id '{:?}' defined.", id,
			).into()})?;
		Ok(external_function)
	}


	pub fn get_external_function_id_by_identity(
		&self, identity: &FunctionIdentity,
	) -> Result<ExternalFunctionId, QuMsg> {
		let first_arg_type = match identity.parameters.first() {
			Some(object_id) => *object_id,
			None => self.class_id::<QuVoid>()?,
		};
		let external_function_id = *self
			.get_class(first_arg_type)?
			.external_functions_map
			.get(&identity)
			.ok_or_else(|| -> QuMsg {format!(
				"There's no function defined with the identity '{:?}'",
				&identity,
			).into()})?;

		Ok(external_function_id)
	}


	pub fn get_function(&self, id: FunctionId) -> Result<&FunctionMetadata, QuMsg> {
		let function = self.functions
			.get(usize::from(id))
			.ok_or_else(|| -> QuMsg { format!(
				"There's no function with id '{:?}' defined.", id,
			).into()})?;
		Ok(function)
	}


	pub fn get_function_mut(
		&mut self, id: FunctionId
	) -> Result<&mut FunctionMetadata, QuMsg> {
		let function = self.functions
			.get_mut(usize::from(id))
			.ok_or_else(|| -> QuMsg { format!(
				"There's no function with id '{:?}' defined.", id,
			).into()})?;
		Ok(function)
	}


	pub fn get_function_id_by_identity(
		&self, identity: &FunctionIdentity,
	) -> Result<FunctionId, QuMsg> {
		let first_arg_type = match identity.parameters.first() {
			Some(object_id) => *object_id,
			None => self.class_id::<QuVoid>()?,
		};
		let function_id = *self
			.get_class(first_arg_type)?
			.functions_map
			.get(&identity)
			.ok_or_else(|| -> QuMsg {format!(
				"There's no function defined with the identity '{:?}'",
				&identity,
			).into()})?;

		Ok(function_id)
	}


	fn get_module(&self, id:ModuleId) -> Result<&ModuleMetadata, QuMsg> {
		if id >= self.modules.len() {
			return Err(format!(
				"There is module at index '{}' defined.", id
			).into());
		}
		Ok(&self.modules[id])
	}


	fn get_module_mut(
		&mut self, id:ModuleId,
	) -> Result<&mut ModuleMetadata, QuMsg> {
		if id >= self.modules.len() {
			return Err(format!(
				"There is module at index '{}' defined.", id
			).into());
		}
		Ok(&mut self.modules[id])
	}


	fn get_module_class_id(
		&self, module_id: ModuleId, class_name: &str,
	) -> Result<ClassId, QuMsg> {
		let module = self.get_module(module_id)?;
		let class_id = *module.class_map
			.get(class_name)
			.ok_or_else(|| -> QuMsg { format!(
				"The module '{}' has no class names '{}'", module.name, class_name
			).into()})?;
		Ok(class_id)
	}


	fn get_module_constant_id(
		&self, module_id: ModuleId, name: &str,
	) -> Result<ConstantId, QuMsg> {
		let module = self.get_module(module_id)?;
		let constant_id = *module.constants_map
			.get(name)
			.ok_or_else(|| -> QuMsg { format!(
				"The module '{}' has no constant named '{}'.",
				module.name,
				name,
			).into()})?;
		Ok(constant_id)
	}


	pub fn get_module_id(&self, module:&str) -> Result<ModuleId, QuMsg> {
		assert!(self.module_map.contains_key(module));
		let string = module.to_owned();
		self.module_map
			.get(module)
			.ok_or_else(|| {format!(
				"There is no module named '{}' defined.", module
			).into()})
			.copied()
	}


	fn get_item(&self, name: &str) -> Option<CodeItem> {
		// Search modules for matching name
		for module in &self.modules {
			if let Some(module_item) = module.index_by_str(name) {
				return Some(module_item.into());
			}
		}

		// Search external items for matching name
		if self.imports.structs_map.contains_key(name) {
			return Some(CodeItem::ExternalClass(self.imports.get_struct_by_str(name).unwrap()));
		}
		for function in &self.imports.fns {
			if function.name == name {
				return Some(CodeItem::ExternalFunction(&function));
			}
		}

		// Name not found, return nothing
		None
	}


	pub fn get_some_function_id_by_identity(
		&self, identity: &FunctionIdentity,
	) -> Result<SomeFunctionId, QuMsg> {
		let first_arg_type = match identity.parameters.first() {
			Some(object_id) => *object_id,
			None => self.class_id::<QuVoid>()?,
		};
		let first_arg = self.get_class(first_arg_type)?;

		if first_arg.functions_map.contains_key(&identity) {
			return Ok(SomeFunctionId::Qu(
				*first_arg.functions_map.get(&identity).unwrap()
			));
		} else if  first_arg.external_functions_map.contains_key(&identity) {
			return Ok(SomeFunctionId::External(
				*first_arg.external_functions_map.get(&identity).unwrap()
			));
		}

		Err(format!(
			"There's no function with identity '{:?}' defined.", &identity,
		).into())
	}


	fn find_constant_id(&self, name: &str) -> Option<ConstantId> {
		for module in &self.modules {
			if module.constants_map.contains_key(name) {
				return Some(*module.constants_map.get(name).unwrap());
			}
		}
		None
	}

	
	fn find_class_id(&self, name: &str) -> Result<ClassId, QuMsg> {
		for module in &self.modules {
			if module.class_map.contains_key(name) {
				return Ok(*module.class_map.get(name).unwrap());
			}
		}
		Err(format!(
			"There's no class with name '{}' defined.", name,
		).into())
	}


	/// Registers the functions of the previously registered structs to be used
	/// in the Qu language.
	/// 
	/// # Note
	/// 
	/// A struct must be registered with [`Qu::register_struct`] before its
	/// functions can be registered.
	pub fn register_functions(&mut self) -> Result<(), QuMsg> {
		let mut registrations = vec![];
		for class_id in &self.with_unregistered_functions {
			registrations.push(self.get_class(*class_id)?.register_fn);
		}
		for registration in registrations {
			for external_function in (registration)() {
				self.register_function(external_function)?;
			}
		}
		self.with_unregistered_functions.clear();
		Ok(())
	}


	/// Registers an [`ExternalFunction`] to Qu.
	pub fn register_function(
		&mut self, external_function:ExternalFunction,
	) -> Result<(), QuMsg> {
		// Make function identity
		let mut parameters = vec![];
		for parameter in &external_function.parameters {
			let class_id = self.find_class_id(
				parameter,
			).unwrap();
			parameters.push(class_id);
		}
		let return_type = *parameters.first().unwrap();
		let function_identity = FunctionIdentity {
			name: external_function.name.clone(),
			parameters,
			return_type,
		};

		// Get class id of first parameter
		let class_id = match function_identity.parameters.first() {
			Some(class_id) => *class_id,
			None => {todo!()},
		};

		// Add identity to class's map
		let new_function_id = self.external_functions.len();
		let class = self.get_class_mut(class_id)?;
		assert!(
			!class.external_functions_map.contains_key(&function_identity)
		);
		class.external_functions_map.insert(
			function_identity,
			new_function_id,
		);

		// Add external function to list
		self.external_functions.push(external_function);

		Ok(())
	}


	/// Registers an external struct to be used within the Qu langauge.
	/// 
	/// # Note
	/// 
	/// Does not register the methods of the struct. Call
	/// [`Qu::register_struct`] to register the methods of all registered
	/// structs.
	/// 
	/// # Example
	/// 
	/// ```
	/// use qu::Qu;
	/// use qu::QuRegisterStruct;
	/// 
	/// struct MyStruct();
	/// impl QuRegisterStruct for MyStruct {
	/// 	fn name() -> &'static str {
	/// 		"MyStruct"
	/// 	}
	/// }
	/// 
	/// let mut qu = Qu::new();
	/// qu.register_struct::<MyStruct>();
	/// ```
	pub fn register_module_struct<S:QuRegisterStruct+'static>(
		&mut self, module_id: ModuleId,
	) -> Result<(), QuMsg> {
		let class_name = <S as QuRegisterStruct>::name();

		// Manage classes map
		let class_id:ClassId = self.classes.len().into();
		let module = self.get_module_mut(module_id)?;
		assert!(!module.class_map.contains_key(class_name));
		module.class_map.insert(class_name.into(), class_id);
		self.with_unregistered_functions.push(class_id);

		// Add class to list of classes
		let class = QuStruct::new(
			class_name,
			&<S as QuRegisterStruct>::register_fns,
			size_of::<S>(),
		);
		self.classes.push(class);

		Ok(())

	}

}


#[derive(Debug, Default, Clone)]
pub struct ExternalClassMetadata {
	functions_map: HashMap<String, ExternalFunctionId>,
	/// The name of this class.
	name: String,
	/// The ID of this class.
	id: usize,
}


pub type ExternalFunctionId = usize;


#[derive(Debug, Default, Clone)]
pub struct ExternalFunctionMetadata {
	functions_map: HashMap<String, ExternalFunctionId>,
	/// The name of this class.
	name: String,
	/// The ID of this class.
	id: usize,
}


pub type FunctionId = usize;


#[derive(Debug, Default, Clone)]
pub struct FunctionIdentityMap {
	map: HashMap<String, HashMap<ClassId, Vec<FunctionIdentity>>>,
} impl FunctionIdentityMap {
}


pub type ModuleId = usize;


#[derive(Debug, Default, Clone)]
/// An item that is owned by a Qu module. See [`ModuleMetadata`].
enum ModuleItem {
	Class(Box<ClassMetadata>),
	#[default]
	Constant,
	Function(Box<FunctionMetadata>),
	StaticVariable,
}


pub type ModuleItemId = usize;


#[derive(Debug, Clone)]
/// Reresents a Qu module.
pub struct ModuleMetadata {
	constants_map: HashMap<String, ConstantId>,
	class_map: HashMap<String, ClassId>,
	external_functions_map: HashMap<FunctionIdentity, ExternalFunctionId>,
	functions_map: HashMap<FunctionIdentity, ConstantId>,

	/// The name of this module.
	name: String,
	/// The ID of this module.
	id: ModuleId,
	/// A map of names to class and function IDs.
	id_map: HashMap<String, ModuleItemId>,
	/// A list of all compiled classes, functions, and constants in the module.
	items: Vec<ModuleItem>,
} impl ModuleMetadata {
	fn define_function(&mut self, identity:FunctionIdentity) -> &mut FunctionMetadata{
		assert!(!self.id_map.contains_key(&identity.name));
		let id = self.items.len();
		let fn_name = identity.name.clone();
		self.items.push(ModuleItem::Function(Box::new(FunctionMetadata {
			identity,
			id,
			..Default::default()
		})));
		self.id_map.insert(fn_name, id);
		let ModuleItem::Function(function) = &mut self.items[id] else {panic!("")};
		function
	}


	fn index_by_str(&self, name: &str) -> Option<&ModuleItem> {
		if !self.id_map.contains_key(name) {
			return None;
		}

		let id = self.id_map[name];
		Some(&self.items[id])
	}
} impl Default for ModuleMetadata {
	fn default() -> Self {
		Self {
			class_map: Default::default(),
			constants_map : Default::default(),
			name: "__main__".into(),
			id : Default::default(),
			id_map : Default::default(),
			items : Default::default(),
			external_functions_map: Default::default(),
			functions_map: Default::default(),
		}
	}
}


#[derive(Debug, Clone)]
enum CodeItem<'a> {
	Class(&'a ClassMetadata),
	Constant,
	ExternalFunction(&'a ExternalFunction),
	ExternalClass(&'a QuStruct),
	Function(&'a FunctionMetadata),
	Variable,
} impl<'a> From<&'a ModuleItem> for CodeItem<'a> {
	fn from(value: &'a ModuleItem) -> Self {
		match value {
			ModuleItem::Class(class_metadata) => {
				CodeItem::Class(&class_metadata)
			},
			ModuleItem::Constant => {
				CodeItem::Constant
			},
			ModuleItem::Function(function_metadata) => {
				CodeItem::Function(&function_metadata)
			},
			ModuleItem::StaticVariable => {
				CodeItem::Variable
			},
		}
	}
}


#[derive(Clone, Debug, Default, Eq, Ord)]
pub struct FunctionIdentity {
	pub name: String,
	pub parameters: Vec<ClassId>,
	pub return_type: ClassId,
} impl Hash for FunctionIdentity {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.name.hash(state);
        self.parameters.hash(state);
    }
} impl PartialEq for FunctionIdentity {
    fn eq(&self, other: &Self) -> bool {
        self.name == other.name
			&& self.parameters == other.parameters
    }
} impl PartialOrd for FunctionIdentity {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        match self.name.partial_cmp(&other.name) {
            Some(core::cmp::Ordering::Equal) => {}
            ord => return ord,
        }
        self.parameters.partial_cmp(&other.parameters)
    }
}


#[derive(Debug, Default, Clone)]
pub struct FunctionMetadata {
	id: ModuleItemId,
	pub identity: FunctionIdentity,
	/// The value that the VM's program counter should be set to in order to
	/// start this function.
	pub pc_start: usize,
}


#[derive(Debug, Clone)]
pub enum SomeFunctionId {
	Qu(FunctionId),
	External(ExternalFunctionId),
}


struct QuCmpContext {
	var_identities:Vec<QuVarIdentity>,
	stack_frames:Vec<QuCmpFrame>,
} impl QuCmpContext {
	fn new() -> Self {
		return Self{
			var_identities: Vec::default(),
			stack_frames: Vec::default(),
		};
	}
}


struct QuCmpFrame {
	var_refs_added:u8,
	stack_idx:u8,
} impl QuCmpFrame {
	fn new(stack_idx:u8) -> Self {
		return Self{
			var_refs_added: 0,
			stack_idx,
		};
	}
}


/// Compiles [QuLeaf]s into Qu bytecode.
pub struct QuCompiler {
	contexts:Vec<QuCmpContext>,
	name_refs:HashMap<String, u32>,
	types_map:HashMap<String, usize>,
	definitions: Definitions,
} impl QuCompiler {
	/// Creates and returns a new [QuCompiler].
	pub fn new() -> Self {
		let mut inst = Self {
			contexts: Vec::default(),
			name_refs: HashMap::default(),
			types_map: HashMap::new(),
			definitions: Definitions::default(),
		};

		return inst;
	}


	fn add_name_ref(&mut self, name:String) {
		self.name_refs.insert(name, self.name_refs.len() as u32);
	}


	fn add_variable(&mut self, var_identity:QuVarIdentity) {
		self.contexts_get_top().var_identities.push(var_identity);
		self.context_get_top_frame().var_refs_added += 1;
	}


	/// Compiles an expression into bytecode.
	fn cmp_expr(
		&mut self,
		expression:&Expression,
		output_reg:QuStackId,
		definitions: &mut Definitions,
	) -> Result<QuAsmBuilder, QuMsg> {
		let builder = match expression {
			Expression::Call(
				call_expression,
			) => {
				let mut parameters = vec![];
				for parameter in &call_expression.parameters.elements {
					parameters.push(parameter);
				}
				self.cmp_fn_call(
					call_expression.name.slice.clone(),
					&parameters,
					output_reg,
					definitions,
				)
			}
			Expression::Operation(
				operation_expression,
			) => self.cmp_expr_operation(
				&operation_expression.operator,
				&operation_expression.left,
				&operation_expression.right,
				output_reg,
				definitions,
			),
			Expression::Number(number)
				=> self.cmp_expr_int(&number.value, output_reg, definitions),
			Expression::Tuple(tuple)
				=> self.cmp_expr_tuple(
					&tuple.elements,
					output_reg,
					definitions,
				),
			Expression::Var(var)
				=> self.cmp_expr_val(&var.name, output_reg, definitions),
		}?;

		// Type check
		let void_id = definitions.class_id::<QuVoid>()?;
		if builder.return_type != "void" {
			let returning_type = definitions
				.find_class_id(&builder.return_type)?;
			if returning_type != output_reg.class_id() {
				return Err(format!(
					"Attempted to assign a value of type '{}' to a location of type '{}'.",
					builder.return_type,
					definitions.get_class(output_reg.class_id())?.name,
				).into());
			}
		}

		Ok(builder)
	}


	/// Compiles a math or logic expression into bytecode.
	fn cmp_expr_operation(
		&mut self,
		operator: &QuToken,
		left: &Expression,
		right: &Expression,
		// TODO: Change output_reg to a struct without type information
		output_reg: QuStackId,
		definitions: &mut Definitions,
	)-> Result<QuAsmBuilder, QuMsg> {
		self.cmp_fn_call(
			QuOperator::from(operator.slice.as_str()).name().into(),
			&vec![left, right],
			output_reg,
			definitions,
		)
	}


	/// Compiles a constant integer expression into bytecode.
	/// 
	/// # Panics
	/// 
	/// Panics if `val` can't be parsed to a number.
	fn cmp_expr_int(
		// TODO: Change output_reg to a struct without type information
		&mut self,
		value:&QuToken,
		output_reg:QuStackId,
		definitions: &mut Definitions,
	) -> Result<QuAsmBuilder, QuMsg> {
		// TODO: Support other int sizes
		
		let Ok(val) = value.slice.parse::<isize>() else {
			panic!("Could not convert text '{}' to number!", value.slice);
		};

		let mut b = QuAsmBuilder::new();
		b.add_op(Value(val, output_reg));
		b.return_type = definitions.get_class(
			definitions.class_id::<i32>()?,
		)?.name.clone();

		return Ok(b);
	}


	/// Compiles a tuple construction into bytecode.
	fn cmp_expr_tuple(
		&mut self,
		elements:&Vec<Expression>,
		to_reg:QuStackId,
		definitions: &mut Definitions,
	) -> Result<QuAsmBuilder, QuMsg> {
		let mut b = QuAsmBuilder::new();
		let mut i:u8 = to_reg.into();
		for item in elements {
			b.add_builder(self.cmp_expr(item, i.into(), definitions)?);
			i += 1;
		}

		return Ok(b);
	}


	/// Compiles a variable-expression into bytecode.
	fn cmp_expr_val(
		&mut self,
		name:&QuToken,
		output_reg:QuStackId,
		definitions: &mut Definitions,
	) -> Result<QuAsmBuilder, QuMsg> {

		// TODO: Error handling
		let var_identity = self.get_var_identity(&name.slice).unwrap();
		
		// Copying to same register location, return nothing
		if var_identity.stack_id == output_reg {
			let mut b = QuAsmBuilder::default();
			b.return_type = var_identity.static_type.clone();
			return Ok(b);
		}

		let identity = &FunctionIdentity {
			name: "copy".into(),
			parameters: vec![var_identity.stack_id.class_id()],
			return_type: var_identity.stack_id.class_id(),
		};

		let mut b = QuAsmBuilder::new();
		b.add_op(QuOp::CallExt(
			definitions.get_external_function_id_by_identity(identity)?,
			vec![var_identity.stack_id],
			output_reg,
		));

		b.return_type = var_identity.static_type.clone();

		return Ok(b);
	}


	/// Compiles an *if* statement into bytecode.
	fn cmp_flow_if(
		&mut self,
		mut condition:&Expression,
		body:&CodeScope,
		definitions: &mut Definitions,
	) -> Result<QuAsmBuilder, QuMsg> {
		// Get expression register
		let mut expr_b = with!{
			self.stack_frame_start, self.stack_frame_end {
				let expr_reg = self.get_expr_reg(&mut condition, definitions)?;
				self.cmp_expr(
					condition,
					QuStackId::new(
						expr_reg.index(),
						definitions.class_id::<bool>()?,
					),
					definitions
				)
			}
		}?;

		// New frame for the code in the 'if' body
		let b:Result<QuAsmBuilder, QuMsg> = with!{
			self.stack_frame_start, self.stack_frame_end {
				let body_code = self.cmp_scope(body, definitions)?;

				let mut b = QuAsmBuilder::new();
				b.add_op(JumpByIfNot(body_code.len() as isize));
				b.add_builder(body_code);

				/*return*/ Ok(b)
			}
		};

		expr_b.add_builder(b?);

		return Ok(expr_b);
	}


	/// Compiles a *while* statement into bytecode.
	fn cmp_flow_while(
		&mut self,
		mut condition:&Expression,
		body:&CodeScope,
		definitions: &mut Definitions,
	) -> Result<QuAsmBuilder, QuMsg> {
		// Get expression register
		let expr_code = with!(
			self.stack_frame_start, self.stack_frame_end {
				let if_expr_reg = self.get_expr_reg(&mut condition, definitions)?;
				// Expression code
				self.cmp_expr(
					condition,
					QuStackId::new(
						if_expr_reg.index(),
						definitions.class_id::<bool>()?,
					),
					definitions
				)
			}
		)?;

		// New frame for the code in the 'if' body
		let code= with!{
			self.stack_frame_start, self.stack_frame_end {
				// --- Compile Pieces ---
				// Code block
				let mut block_code = self.cmp_scope(body, definitions)?;
				let block_code_len = block_code.len();

				let mut b = QuAsmBuilder::new();

				// Loop check expression
				let expr_code_len = expr_code.len();
				b.add_builder(expr_code);

				// Skip body if expression false
				let mut skip_body_b = QuAsmBuilder::new();
				skip_body_b.add_op(JumpByIfNot(block_code_len as isize + 1));
				let skip_body_b_len = skip_body_b.len();
				b.add_builder(skip_body_b);

				// Loop body
				b.add_builder(block_code);

				// Jump back to expression
				b.add_op(JumpBy(-((
					block_code_len
					+ skip_body_b_len
					+ expr_code_len
					+ 1
				) as isize)));

				Ok::<QuAsmBuilder, QuMsg>(b)
			}
		}?;
		
		return Ok(code);
	}


	fn cmp_fn_call(
		&mut self,
		name: String,
		parameters: &Vec<&Expression>,
		store_to: QuStackId,
		definitions: &mut Definitions,
	) -> Result<QuAsmBuilder, QuMsg> {
		let mut builder = QuAsmBuilder::new();

		let mut function_identity = FunctionIdentity {
			name,
			return_type: store_to.class_id(),
			..Default::default()
		};

		let mut parameter_ids = vec![];
		with!(self.stack_frame_start, self.stack_frame_end {
			// Compile parameter expressions
			for parameter in parameters {
				let object_id = self.get_expr_type(parameter, definitions)?;
				function_identity.parameters.push(object_id);

				let reg = self.stack_reserve(object_id, definitions)?;
				let parameter_expr = self.cmp_expr(parameter, reg, definitions)?;
				builder.add_builder(parameter_expr);
				parameter_ids.push(reg);
			}
			Ok::<(), QuMsg>(())
		})?;

		match definitions.get_some_function_id_by_identity(&function_identity)? {
			SomeFunctionId::Qu(function_id) => {
				builder.add_op(QuOp::Call(function_id, store_to));
			},
			SomeFunctionId::External(external_function_id) => {
				builder.add_op(QuOp::CallExt(
					external_function_id,
					parameter_ids,
					store_to
				));
			},
		}

		return Ok(builder);
	}


	/// Compiles a function declaration.
	/// 
	/// Note
	/// 
	/// The pc location of the function is determined by the vm upon excecuting
	/// a function declaration operation.
	fn cmp_fn_decl(
		&mut self,
		identity: &crate::parser::parsed::FunctionIdentity,
		body: &CodeScope,
		definitions: &mut Definitions,
	) -> Result<QuAsmBuilder, QuMsg> {
		self.add_name_ref(identity.name.slice.clone());

		let needs_added_end_op = match body.code_block.statements.last() {
			Some(Statement::Return(_)) => false,
			_ => true,
		};

		let body_code
			= with!(self.context_start, self.context_end {

			// Add return value variable for padding.
			let literals = definitions.get_module_id(BASE_MODULE)?;
			let object_id = definitions.get_module_class_id(literals, "int")?;
			let id = self.stack_reserve(object_id, definitions)?;
			self.add_variable(QuVarIdentity::new(
				"return value".to_owned(),
				"int".to_owned(),
				id,
			));

			// Compile parameters
			for p in &identity.parameters {
				let static_type = match &p.static_type {
					Some(token) => token.slice.clone(),
					None => "int".to_owned(),
				};
				let object_id = definitions.find_class_id(&static_type)?;
				let id = self.stack_reserve(object_id, definitions)?;
				self.add_variable(QuVarIdentity::new(
					p.name.slice.to_owned(),
					static_type.to_owned(),
					id,
				));
			}

			// Compile code block
			let mut b = QuAsmBuilder::new();
			b.add_builder(self.cmp_code_block(&body.code_block, definitions)?);
			if needs_added_end_op {
				b.add_op(End);
			}
			Ok::<QuAsmBuilder, QuMsg>(b)
		})?;

		// Make identity
		let mut parameters = vec![];
		for parameter in &identity.parameters {
			let static_type = &parameter.static_type
				.as_ref()
				.expect("Not yet implemented dynamic typing")
				.slice;
			parameters.push(definitions.find_class_id(&static_type)?);
			
		}
		let return_type_name = &identity.return_type
			.as_ref()
			.expect("Not yet implemented dynamic typing")
			.slice;
		let return_type = definitions.find_class_id(&return_type_name)?;
		let identity = FunctionIdentity {
			name: identity.name.slice.clone(),
			parameters,
			return_type,
		};

		// Add fn declaration operation
		let mut code = QuAsmBuilder::new();
		let func_id = definitions
			.get_function_id_by_identity(&identity)?;
	
		code.add_op(DefineFn(func_id, body_code.len() as usize));

		// Add body
		code.add_builder(body_code);

		return Ok(code);
	}


	/// Compiles a [QuLeaf] into bytecode.
	fn cmp_statement(
		&mut self,
		statement:&Statement,
		definitions: &mut Definitions,
	) -> Result<QuAsmBuilder, QuMsg> {
		match statement {
			Statement::Expression(expression) => {
				return with!(self.stack_frame_start, self.stack_frame_end {
					let reg = self.get_expr_reg(&expression, definitions)?;
					self.cmp_expr(expression, reg, definitions)
				});
			}
			Statement::FlowStatement(flow_statement) => {
				match flow_statement.flow_keyword.slice.as_str() {
					KEYWORD_IF => {
						return self.cmp_flow_if(
							&flow_statement.condition,
							&flow_statement.body,
							definitions,
						);
					},
					KEYWORD_WHILE => {
						return self.cmp_flow_while(
							&flow_statement.condition,
							&flow_statement.body,
							definitions,
						);
					}
					_ => unimplemented!(),
				}
			}
			Statement::FunctionDeclaration(function_declaration) => {
				// TODO: Compiler fn declaration parameters
				return self.cmp_fn_decl(
					&function_declaration.identity,
					&function_declaration.body,
					definitions,
				);
			}
			Statement::Return(return_statement) => {
				let mut code = QuAsmBuilder::new();
				match &return_statement.value {
					Some(expression) => {
						let return_type = self.get_expr_type(
							expression,
							definitions,
						)?;
						let return_reg = QuStackId::new(
							0,
							return_type,
						);
						let reg = self.stack_reserve(
							return_type,
							definitions,
						)?;
						let copy_idenity = FunctionIdentity {
							name: "copy".into(),
							parameters: vec![return_type],
							return_type: return_type,
						};
						
						code.add_builder(self.cmp_expr(
							expression,
							reg,
							definitions
						)?);
						code.add_op(QuOp::CallExt(
							definitions.get_external_function_id_by_identity(&copy_idenity)?,
							vec![reg],
							return_reg,
						));

						if self.contexts.len() == 1 {
							code.add_op(Return(return_type));
						} else {
							code.add_op(End);
						}
					},
					None => {},
				}
				return Ok(code);
			}
			Statement::VarDeclaration(var_declaration) => {
				return self.cmp_var_decl(
					&var_declaration.name,
					&var_declaration.static_type,
					&var_declaration.initial_value,
					definitions
				);
			}
			Statement::VarAssign(variable_assignment) => {
				return  self.cmp_var_assign(
					&variable_assignment.name,
					&variable_assignment.new_value,
					definitions,
				);
			}
		};
	}


	/// Compiles a [`Vec<QuLeaf>`] into bytecode.
	fn cmp_code_block(
		&mut self,
		code_block: &CodeBlock,
		definitions: &mut Definitions,
	) -> Result<QuAsmBuilder, QuMsg> {
		let mut b = QuAsmBuilder::new();
		for statements in &code_block.statements {
			b.add_builder(self.cmp_statement(statements, definitions)?);
		}
		return Ok(b);
	}


	fn cmp_module(&mut self) {
		
	}


	/// Compiles code variable assignment.
	fn cmp_var_assign(
		&mut self,
		name:&QuToken,
		new_value:&Expression,
		definitions: &mut Definitions,
	) -> Result<QuAsmBuilder, QuMsg> {
		// Get variable register
		let var_identity
			= self.get_var_identity(&name.slice)
				.ok_or_else(||{
					let mut msg = QuMsg::undefined_var_assign(
						&name.slice);
					msg.token = name.char_index.clone();
					return msg;
				}
			)?
		;
		let var_id = var_identity.stack_id.clone();
		// Compile assignment to expression
		return self.cmp_expr(new_value, var_id, definitions);
	}


	/// Compiles a variable declaration.
	fn cmp_var_decl(
		&mut self,
		name:&QuToken,
		static_type:&Option<QuToken>,
		initial_value:&Option<Expression>,
		definitions: &mut Definitions,
	) -> Result<QuAsmBuilder, QuMsg> {

		// Check if the variable is already defined
		if self.is_var_defined(&name.slice) {
			let mut msg = QuMsg::var_redefined(&name.slice);
			msg.token = name.char_index.clone();
			return Err(msg);
		}

		// Get static type
		let object_id = match static_type {
			Some(type_tk) => {
				definitions.find_class_id(&type_tk.slice)?
			},
			None => {
				match initial_value {
					Some(expression) => {
						self.get_expr_type(expression, definitions)?
					},
					None => definitions.class_id::<i32>()?,
				}
			},
		};

		// Create variable
		let var_reg = self.stack_reserve(object_id, definitions)?;
		self.add_variable(QuVarIdentity {
			name: name.slice.to_owned(),
			static_type: definitions.get_class(object_id)?.name.clone(),
			stack_id: var_reg,
		});

		// Compile variable assignment
		return match initial_value {
			// Compile variable value
			Some(expression)
				=> self.cmp_expr(expression, var_reg, definitions),

			// No default value, compile fallback to zero
			None => {
				// No default value, compile fallback to zero
				let mut code = QuAsmBuilder::new();
				code.add_op(Value(0, var_reg));
				Ok(code)
			},
		};
	}


	/// Compiles a scope.
	fn cmp_scope(
		&mut self,
		code_scope: &CodeScope,
		definitions: &mut Definitions,
	) -> Result<QuAsmBuilder, QuMsg> {
		let compiled
			= with!{self.stack_frame_start, self.stack_frame_end {
				self.cmp_code_block(&code_scope.code_block, definitions)
			}
		};
		return compiled;
	}


	/// Compiles Qu code from a [`&str`] into a [`Vec<u8>`].
	pub fn compile(
		&mut self, code:&str, definitions: &mut Definitions,
	) -> Result<Vec<QuOp>, QuMsg> {
		let mut p = QuParser::new();
		let code_block = p.parse(code)?;

		self.prepass(&code_block, definitions);
		let compiled = self.compile_code(&code_block, definitions)?;
		Ok((compiled))
	}


	/// Compiles Qu code from a [QuLeaf] into a [`Vec<u8>`].
	pub fn compile_code(
		&mut self, code_block:&CodeBlock, definitions: &mut Definitions
	) -> Result<Vec<QuOp>, QuMsg> {
		// Main code
		let mut code = with!(self.context_start, self.context_end {
			let mut code = self.cmp_code_block(code_block, definitions)?;
			code.add_op(End);
			Ok::<QuAsmBuilder, QuMsg>(code)
		})?;


		Ok(code.compile(
			&self.name_refs,
			definitions,
		)?)
	}


	fn context_end(&mut self) {
		self.stack_frame_end();
		self.contexts.pop().unwrap();
	}


	fn context_get_top_frame(&mut self) -> &mut QuCmpFrame{
		let c_top = self.contexts_get_top();
		if c_top.stack_frames.len() == 0 {
			panic!();
		}
		let l = c_top.stack_frames.len();
		return &mut c_top.stack_frames[l-1];
	}


	fn context_get_top_frame_option(&mut self) -> Option<&mut QuCmpFrame> {
		let c_top = self.contexts_get_top();
		if c_top.stack_frames.len() == 0 {
			return None;
		}
		let l = c_top.stack_frames.len();
		return Some(&mut c_top.stack_frames[l-1]);
	}


	fn context_start(&mut self) {
		let c = QuCmpContext::new();
		self.contexts.push(c);
		self.stack_frame_start();
	}


	fn contexts_get_top(&mut self) -> &mut QuCmpContext{
		if self.contexts.len() == 0 {
			panic!();
		}
		let l = self.contexts.len();
		return &mut self.contexts[l-1];
	}


	/// Returns an appropriate location to store an expression.
	/// 
	/// Most expressions require a new memory location, but variables
	/// should just return the register of the variable.
	fn get_expr_reg(
		&mut self, expr_leaf:&Expression, definitions: &mut Definitions
	) -> Result<QuStackId, QuMsg> {
		return  match expr_leaf {
			Expression::Operation(_) => Ok(self.stack_reserve(definitions.class_id::<i32>()?, definitions)?),
			Expression::Call(_) => Ok(self.stack_reserve(definitions.class_id::<i32>()?, definitions)?),
			Expression::Number(_) => Ok(self.stack_reserve(definitions.class_id::<i32>()?, definitions)?),
			Expression::Tuple(_) => Ok(self.stack_reserve(definitions.class_id::<i32>()?, definitions)?),
			Expression::Var(var) => {
				self.get_var_register(&var.name.slice)
					.ok_or_else(||{
						let mut msg = QuMsg::undefined_var_access(
							&var.name.slice
						);
						msg.token = var.name.char_index.clone();
						return msg;
					}
				)
			}
		};
	}


	/// Returns the static type of an expression.
	fn get_expr_type(
		&mut self, expr_leaf:&Expression, definitions: &mut Definitions
	) -> Result<ClassId, QuMsg> {
		let object_id = match expr_leaf {
			Expression::Operation(operation) => {
				let identity = FunctionIdentity {
					name: QuOperator::from(operation.operator.slice.as_str())
						.name()
						.into(),
					parameters: vec![
						self.get_expr_type(&operation.left, definitions)?,
						self.get_expr_type(&operation.right, definitions)?,
					],
					..Default::default()
				};

				let some_function_id = definitions
					.get_some_function_id_by_identity(&identity)?;
				let return_type_id = match some_function_id {
					SomeFunctionId::Qu(id) => {
						let return_type_id = definitions
							.get_function(id)?
							.identity
							.return_type;
						return_type_id
					},
					SomeFunctionId::External(id) => {
						let return_type_id = definitions
							.get_external_function(id)?
							.return_type;
						let return_type_id = definitions
							.find_class_id(return_type_id)?;
						return_type_id
					},
				};

				return_type_id
			},
			Expression::Call(call_expr) => {
				let mut parameters = vec![];
				for parameter in &call_expr.parameters.elements {
					parameters.push(self.get_expr_type(
						parameter,
						definitions,
					)?);
				}
				let identity = FunctionIdentity {
					name: call_expr.name.slice.clone(),
					parameters,
					..Default::default()
				};
				let some_function_id = definitions
					.get_some_function_id_by_identity(&identity)?;
				let return_type_id = match some_function_id {
					SomeFunctionId::Qu(id) => {
						let return_type_id = definitions
							.get_function(id)?
							.identity
							.return_type;
						return_type_id
					},
					SomeFunctionId::External(id) => {
						let return_type_id = definitions
							.get_external_function(id)?
							.return_type;
						let return_type_id = definitions
							.find_class_id(return_type_id)?;
						return_type_id
					},
				};

				return_type_id
			},
			Expression::Number(_) => {
				definitions.class_id::<i32>()?
			},
			Expression::Tuple(tuple) => {
				match tuple.elements.get(0) {
					Some(that) => {
						self.get_expr_type(that, definitions)?
					},
					None => {
						definitions.class_id::<QuVoid>()?
					},
				}
			},
			Expression::Var(var) => {
				self.get_var_identity(&var.name.slice)
					.ok_or_else(|| {
						let mut msg = QuMsg::undefined_var_access(&var.name.slice);
						msg.token = var.name.char_index.clone();
						return msg;
					})?.object_id()
			}
		};
		Ok(object_id)
	}


	/// Returns the current stack index.
	fn get_stack_idx(&mut self) -> u8 {
		return self.get_stack_idx_option().unwrap();
	}


	/// Returns the current stack index or [`None`].
	fn get_stack_idx_option(&mut self) -> Option<u8> {
		let Some(f) = self.context_get_top_frame_option()
			else {return None};
		return Some(f.stack_idx);
	}


	/// Gets the identity of a variable by the variable's name.
	fn get_var_identity(&mut self, var_name:&str) -> Option<&QuVarIdentity> {
		let mut i = 0usize;
		for var_ref in &self.contexts_get_top().var_identities {
			if var_ref == var_name {
				return Some(var_ref);
			}
			i += 1;
		}
		return None;
	}


	/// Gets the pointer to a variable by the variable's name.
	fn get_var_register(&mut self, var_name:&str) -> Option<QuStackId> {
		let mut i = 0usize;
		for var_ref in &self.contexts_get_top().var_identities {
			if var_ref == var_name {
				return Some(var_ref.stack_id);
			}
			i += 1;
		}
		return None;
	}


	/// Returns true if the given variable is already defined.
	fn is_var_defined(&mut self, var_name:&str) -> bool {
		// TODO: Maybe use a faster algorithm?
		for var_ref in &self.contexts_get_top().var_identities {
			if var_ref == var_name {
				return true;
			}
		}
		return false;
	}


	fn prepass(
		&mut self, code_block:&CodeBlock, definitions:&mut Definitions,
	) -> Result<(), QuMsg> {
		if !definitions.module_map.contains_key("__main__") {
			definitions.define_module(
				"__main__".into(),
				&|_| {Ok(())},
			)?;
		}

		// Function definitions
		for statement in &code_block.statements {
			match statement {
				Statement::FunctionDeclaration(function_declaration) => {
					let identity = &function_declaration
						.identity;
					let return_type = match &identity.return_type {
						Some(token) => {
							token.slice.clone()
						},
						None => panic!("Dynamic typing not implemented yet"),
					};

					// Make function identity
					let mut parameters = vec![];
					for parameter in &identity.parameters {
						parameters.push(definitions.find_class_id(
							&parameter.static_type
								.as_ref()
								.expect("Dynamic typing not implemented yet")
								.slice
						)?);
					}
					let return_type = definitions.find_class_id(
						&return_type
					)?;
					let function_identity = FunctionIdentity {
						name: identity.name.slice.clone(),
						parameters,
						return_type,
					};

					// Add function to definitions
					definitions.define_function(
						function_identity,
					)?;
				},
				_ => {},
			}
		}

		Ok(())
	}


	/// Closes the current stack frame returning the stake frame to the
	/// beginning of the frame.
	fn stack_frame_end(&mut self) {
		let context = self.contexts_get_top();
		let frame = context.stack_frames.pop().unwrap();
		context.var_identities.resize(
			context.var_identities.len()-(frame.var_refs_added as usize),
			QuVarIdentity::default(),
		);
	}


	/// Starts a new stack frame.
	fn stack_frame_start(&mut self) {
		let f = QuCmpFrame::new(
			self.get_stack_idx_option().unwrap_or(0)
		);
		self.contexts_get_top().stack_frames.push(f);
	}


	/// Returns the current stack pointer and increments it.
	fn stack_reserve(
		&mut self,
		object: ClassId,
		definitions:&mut Definitions,
	) -> Result<QuStackId, QuMsg> {
		let object_size = definitions
			.get_class(object)?
			.size;
		let mut top_frame = self.context_get_top_frame();
		let index = top_frame.stack_idx;
		top_frame.stack_idx += object_size;
		return Ok(QuStackId::new(index as usize, object));
	}

}


#[derive(Debug, Clone)]
enum QuBuilderPiece {
	ReprCall(String, QuStackId),
	// Struct name, fn name, args, output
	//ReprCallExt(String, Vec<QuStackId>, QuStackId),
	/// A [`Vec<u8>`] of code.
	Ops(Vec<QuOp>),
} impl QuBuilderPiece {

	fn len(&self) -> usize{
		match self {
			QuBuilderPiece::ReprCall(_, _) => 1,
			//QuBuilderPiece::ReprCallExt(_, _, _) => 1,
			QuBuilderPiece::Ops(v) => v.len(),
		}
	}

}


#[derive(Debug, Default, Clone)]

struct QuAsmBuilder {
	code_pieces:Vec<QuBuilderPiece>,
	return_type:String,
} impl QuAsmBuilder {

	fn new() -> Self {
		return Self {
			code_pieces: vec![],
			return_type: <QuVoid as QuRegisterStruct>::name().into(),
		}
	}
	

	/// Adds a builder piece
	fn add_bp(&mut self, repr:QuBuilderPiece) {
		self.code_pieces.push(repr);
	}


	fn add_builder(&mut self, mut builder:QuAsmBuilder) {
		self.code_pieces.append(&mut builder.code_pieces);
	}


	fn add_op(&mut self, op:QuOp) {
		self.code_pieces.push(QuBuilderPiece::Ops(vec![op]));
	}


	fn add_ops(&mut self, ops:Vec<QuOp>) {
		self.code_pieces.push(QuBuilderPiece::Ops(ops));
	}


	fn compile(
		&mut self,
		name_references: &HashMap<String, u32>,
		definitions: &mut Definitions,
	) -> Result<Vec<QuOp>, QuMsg> {
		let mut code = vec![];
		
		for x in &mut self.code_pieces {
			match x {
				QuBuilderPiece::Ops(ops) => code.append(ops),
				QuBuilderPiece::ReprCall(
					name,
					output,
				) => {
					let Some(fn_index) = name_references.get(name) else {
						panic!("Compiler could not find a function by name {name}.");
					};
					panic!();
					//code.push(Call((*fn_index).into(), *output));
				}
//				QuBuilderPiece::ReprCallExt(
//					fn_name,
//					args,
//					output
//				) => {
//					// TODO: Implement static typing
//					let struct_data = definitions.get_class(
//						args.get(0)
//							.unwrap_or(&QuStackId::from(0))
//							.class_id()
//					)?;
//
//					let id
//						= struct_data.get_fn_id(fn_name)?;
//					code.push(CallExt(id, take(args), *output));
//				},
			};
		}
		

		return Ok(code);
	}


	fn len(&self) -> usize {
		let mut l = 0;
		for x in &self.code_pieces {
			l += x.len();
		}
		return l;
	}

}


#[derive(Debug, Default, Clone)]
struct QuVarIdentity {
	name: String,
	static_type: String,
	stack_id: QuStackId,
} impl QuVarIdentity {

	fn new(name:String, static_type:String, id:QuStackId) -> Self {
		QuVarIdentity{name, static_type, stack_id: id}
	}


	fn object_id(&self) -> ClassId {
		self.stack_id.class_id()
	}

} impl PartialEq<str> for QuVarIdentity {

	fn eq(&self, other:&str) -> bool {
		&self.name == other
	} 

}


pub enum CompilerInstruction {
	// (fn_name, args, result_variable)
	CallExt(FunctionIdentity, Vec<String>, String),
	CloseFrame,
	CloseScope,
	// (name, body_size)
	DefineFn(String, usize),
	// (name, type)
	DefineVariable(String, String),
	End,
	JumpBy(isize),
	JumpByIfNot(isize),
	OpenFrame,
	OpenScope,
	// (var_name)
	Return(String),
}
