
use crate::ExternalFunction;
use crate::QuOp;
use crate::QuOp::*;
use crate::QuParser;
use crate::QuRegisterStruct;
use crate::QuStackId;
use crate::QuVoid;
use crate::import::QuRegistered;
use crate::import::QuStruct;
use crate::import::ClassId;
use crate::parser::KEYWORD_IF;
use crate::parser::QuOperator;
use crate::parser::parsed::*;

use crate::QuMsg;
use crate::QuToken;
use crate::vm::BASE_MODULE;
use crate::vm::Stack;

use core::panic;
use std::collections::HashMap;
use std::mem::size_of;
use std::hash::Hash;
use std::path::Iter;

// TODO: Fix compiler's documentation

#[derive(Debug, Default, Clone)]
pub struct GrowingStack {
	allocated: usize,
	data: Vec<u8>,
} impl GrowingStack {

	fn allocate(&mut self, size: usize) -> usize {
		let index = self.allocated;
		self.allocated += size;
		if self.allocated > self.data.len() {
			self.data.resize(self.allocated, 0);
		}
		index
	}

} impl Stack for GrowingStack {
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


/// The context for what is being compiled. Records things like temporary
/// variables and import shortcuts.
#[derive(Debug, Default, Clone)]
struct Context {
	frames: Vec<ContextFrame>,
	variables: Vec<VariableMetadata>,
} impl Context {
	fn allocate(
		&mut self, class:ClassId, definitions: &Definitions
	) -> Result<QuStackId, QuMsg> {
		match self.frames.last_mut().unwrap() {
			ContextFrame::Module(_, frame_data) => {
				let scope = frame_data.scopes.last_mut().unwrap();
				let stack_id = QuStackId::new(
					scope.stack_size,
					class,
				);
				scope.stack_size += definitions
					.get_class(class)?
					.size as usize;
				Ok(stack_id)
			},
			ContextFrame::Class(_, _) => {
				/* Class variables are handled sparately, pass */
				panic!()
			},
			ContextFrame::Function(_, frame_data) => {
				let scope = frame_data.scopes.last_mut().unwrap();
				let stack_id = QuStackId::new(
					scope.stack_size,
					class,
				);
				scope.stack_size += definitions
					.get_class(class)?
					.size as usize;
				Ok(stack_id)
			},
		}
	}


	fn define_variable(
		&mut self,
		name: String,
		static_type: ClassId,
		definitions: &mut Definitions,
	) -> Result<&VariableMetadata, QuMsg> {
		match self.frames.last_mut().unwrap() {
			ContextFrame::Module(_, frame_data) => {
				let scope = frame_data.scopes.last_mut().unwrap();
				scope.variables_map.insert(
					name.clone(),
					self.variables.len()
				);

				let stack_id = self.allocate(
					static_type, definitions,
				)?;
				self.variables.push(VariableMetadata {
					name: name,
					stack_id,
				});

				Ok(self.variables.last().unwrap())
			},
			ContextFrame::Class(_, _) => {
				/* Class variables are handled sparately, pass */
				panic!()
			},
			ContextFrame::Function(_, frame_data) => {
				let scope = frame_data.scopes.last_mut().unwrap();
				scope.variables_map.insert(
					name.clone(),
					self.variables.len()
				);

				let stack_id = self.allocate(
					static_type, definitions,
				)?;
				self.variables.push(VariableMetadata {
					name: name,
					stack_id,
				});

				Ok(self.variables.last().unwrap())
			},
		}
	}


	fn close_frame(&mut self) {
		self.frames.pop();
	}


	fn close_scope(&mut self) {
		let frame = match self.frames.last_mut().unwrap() {
			ContextFrame::Module(_, frame) => frame,
			ContextFrame::Class(_, frame) => frame,
			ContextFrame::Function(_, frame) => frame,
		};
		frame.scopes.pop();
	}


	fn find_variable_id(&self, name: &str) -> Option<VariableId> {
		let frame = match self.frames.last().unwrap() {
			ContextFrame::Module(_, frame) => frame,
			ContextFrame::Class(_, frame) => frame,
			ContextFrame::Function(_, frame) => frame,
		};
		for scope in frame.scopes.iter().rev() {
			match scope.variables_map.get(name) {
				Some(var_id) => return Some(*var_id),
				None => {},
			}
		}
		return None;
	}


	fn get_variable(
		&self, id: VariableId,
	) -> Result<&VariableMetadata, QuMsg> {
		self.variables
			.get(id)
			.ok_or_else(||{format!(
				"Context does not have a variable with id '{id}'"
			).into()})
	}


	fn open_frame(&mut self, frame: ContextFrame) {
		self.frames.push(frame);
	}


	fn open_scope(&mut self) {
		let Some(some_frame) = self.frames.last_mut() else {
			panic!("Can't open a scope because there are no open frames.");
		};
		let frame = match some_frame {
			ContextFrame::Module(_, frame) => {frame},
			ContextFrame::Class(_, _) => {
				/* Classes can't open scopes */
				return
			},
			ContextFrame::Function(_, frame) => {frame},
		};
		let initial_stack_size = match frame.scopes.last() {
			Some(scope) => scope.stack_size,
			None => 0,
		};
		frame.scopes.push(Scope {
			stack_size: initial_stack_size,
    		..Default::default()
		});
	}
}


#[derive(Debug, Clone)]
enum ContextFrame {
	Module(ModuleId, FrameData),
	Class(ClassId, FrameData),
	Function(FunctionId, FrameData),
} impl ContextFrame {
	fn class(id: ClassId) -> ContextFrame {
		ContextFrame::Class(id, FrameData::default())
	}


	fn function(id: FunctionId) -> ContextFrame {
		ContextFrame::Function(id, FrameData::default())
	}


	fn module(id: ModuleId) -> ContextFrame {
		ContextFrame::Module(id, FrameData::default())
	}
} impl Default for ContextFrame {
    fn default() -> Self {
        ContextFrame::Module(0, Default::default())
    }
}


#[derive(Debug, Default, Clone)]
pub struct Definitions {
	pub constants: GrowingStack,
	pub classes: Vec<QuStruct>,
	pub external_functions: Vec<ExternalFunction>,
	pub functions: Vec<FunctionMetadata>,
	pub function_groups: Vec<SomeFunctionGroup>,
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


	pub fn add_class_external_function_map(
		&mut self,
		class_id: ClassId,
		signature: FunctionIdentity,
		id: ExternalFunctionId,
	) -> Result<(), QuMsg> {
		let name = &signature.name;
		let new_id = self.function_groups.len();
		let class = self.get_class_mut(class_id)?;

		// Get group id or create a new group
		let group_id = match &class.function_groups_map.get(name) {
			Some(id) => {**id},
			None => {
				let id = new_id;
				class.function_groups_map.insert(
					name.into(), id,
				);
				self.function_groups.push(HashMap::default());
				id
			}
		};
		let group = &mut self
			.function_groups[group_id];

		// Add to mappings
		assert!(!group.contains_key(&signature));
		group.insert(signature.clone(), SomeFunctionId::External(id)); // TODO: Remove cloning
		self.get_class_mut(class_id)?.external_functions_map.insert(signature, id);
		Ok(())
	}


	pub fn add_class_function_map(
		&mut self,
		class_id: ClassId,
		signature: FunctionIdentity,
		id: FunctionId,
	) -> Result<(), QuMsg> {
		let name = &signature.name;
		let new_id = self.function_groups.len();
		let class = self.get_class_mut(class_id)?;

		// Get group id or create a new group
		let group_id = match &class.function_groups_map.get(name) {
			Some(id) => {**id},
			None => {
				let id = new_id;
				class.function_groups_map.insert(
					name.into(), id,
				);
				self.function_groups.push(HashMap::default());
				id
			}
		};
		let group = &mut self
			.function_groups[group_id];

		// Add to mappings
		assert!(!group.contains_key(&signature));
		group.insert(signature.clone(), SomeFunctionId::Qu(id)); // TODO: Remove cloning
		self.get_class_mut(class_id)?.functions_map.insert(signature, id);

		Ok(())
	}


	pub fn add_module_external_function_map(
		&mut self,
		module_id: ModuleId,
		signature: FunctionIdentity,
		id: ExternalFunctionId,
	) -> Result<(), QuMsg> {
		let name = &signature.name;
		let new_id = self.function_groups.len();
		let module = self.get_module_mut(module_id)?;

		// Get group id or create a new group
		let group_id = match &module.function_groups_map.get(name) {
			Some(id) => {**id},
			None => {
				let id = new_id;
				module.function_groups_map.insert(
					name.into(), id,
				);
				self.function_groups.push(HashMap::default());
				id
			}
		};
		let group = &mut self
			.function_groups[group_id];

		// Add to mappings
		assert!(!group.contains_key(&signature));
		group.insert(signature.clone(), SomeFunctionId::External(id)); // TODO: Remove cloning
		self.get_module_mut(module_id)?.external_functions_map.insert(signature, id);
		Ok(())
	}


	pub fn add_module_function_map(
		&mut self,
		module_id: ModuleId,
		signature: FunctionIdentity,
		id: FunctionId,
	) -> Result<(), QuMsg> {
		let name = &signature.name;
		let new_id = self.function_groups.len();
		let module = self.get_module_mut(module_id)?;

		// Get group id or create a new group
		let group_id = match &module.function_groups_map.get(name) {
			Some(id) => {**id},
			None => {
				let id = new_id;
				module.function_groups_map.insert(
					name.into(), id,
				);
				self.function_groups.push(HashMap::default());
				id
			}
		};
		let group = &mut self
			.function_groups[group_id];

		// Add to module's group mappings
		assert!(!group.contains_key(&signature));
		group.insert(signature.clone(), SomeFunctionId::Qu(id)); // TODO: Remove cloning
		// Add to module's function mappings
		self.get_module_mut(module_id)?.functions_map.insert(signature.clone(), id);// TODO: Remove cloning
		// Add to class's mappings
		let class_id = signature.id_self_class();
		self.add_class_function_map(class_id, signature, id)?;

		Ok(())
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
		self.add_class_function_map(
			first_arg_id, identity.clone(), funciton_id,
		)?;

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
		
		assert_eq!(self.with_unregistered_functions.len(), 0, "Some functions were not registered");
		(body)(ModuleBuilder(self, id))?;
		assert_eq!(self.with_unregistered_functions.len(), 0, "Some functions were not registered");
		
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


	pub fn define_module_function(
		&mut self, module_id: ModuleId, identity: FunctionIdentity,
	) -> Result<FunctionId, QuMsg> {
		let id = FunctionId::from(self.functions.len());
		self.functions.push(FunctionMetadata {
			id,
			identity: identity.clone(),
			..Default::default()
		});
		self.add_module_function_map(module_id, identity, id)?;
		
		Ok(id)
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


	fn has_path_index(
		&self, path: &DotPath, context: &Context,
	) -> Result<bool, QuMsg> {
		let mut frame = &ContextFrame::default();
		for _frame in context.frames.iter().rev() {
			match _frame {
				ContextFrame::Module(_, _) => frame = _frame,
				_ => continue,
			}
		}
		let mut item_id:ItemId = frame.into();
		for element in &path.vec {
			match item_id {
				ItemId::Class(id) => {
					let class = self.get_class(id)?;
					if class.function_groups_map.contains_key(*element) {
						item_id = ItemId::FunctionGroup(
							*class.function_groups_map.get(*element).unwrap()
						);
						continue;
					} else if class.constants_map.contains_key(*element) {
						todo!()
					}

					return Ok(false);
				},
				ItemId::Variable(id) => {
					let var = context.get_variable(id)?;
					let class_id = var.stack_id.class_id();
					let class = self.get_class(class_id)?;
					if class.function_groups_map.contains_key(*element) {
						item_id = ItemId::FunctionGroup(
							*class.function_groups_map.get(*element).unwrap()
						);
						continue;
					} else if class.constants_map.contains_key(*element) {
						todo!()
					}

					return Ok(false);
				},
				ItemId::FunctionGroup(_) => { return Ok(false); },
				ItemId::Module(id) => {
					let module = self.get_module(id)?;
					match module.constants_map.get(*element) {
						Some(constant_id) => {
							todo!();
							continue;
						},
						None => {},
					};
					match module.function_groups_map.get(*element) {
						Some(function_group) => {
							item_id = ItemId::FunctionGroup(*function_group);
							continue;
						},
						None => {},
					};
					match module.class_map.get(*element) {
						Some(class_id) => {
							item_id = ItemId::Class(*class_id);
							continue;
						},
						None => {},
					};
					return Ok(false);
				},
				ItemId::Function(_) => todo!(),
				ItemId::ExternalFunction(_) => todo!(),
			}
		}
		return Ok(true);
	}


	fn path_index(
		&self, path: &DotPath, context: &Context,
	) -> Result<ItemId, QuMsg> {
		let mut frame = &ContextFrame::default();
		for _frame in context.frames.iter().rev() {
			match _frame {
				ContextFrame::Module(_, _) => frame = _frame,
				_ => continue,
			}
		}
		let mut item_id:ItemId = frame.into();
		for element in &path.vec {
			match item_id {
				ItemId::Class(id) => {
					let class = self.get_class(id)?;
					if let Some(id) = class.function_groups_map.get(*element) {
						item_id = ItemId::FunctionGroup(*id);
						continue;
					} else if class.constants_map.contains_key(*element) {
						todo!()
					}

					return Err(format!(
						"Class does not have element '{element}'."
					).into());
				},
				ItemId::Variable(id) => {
					let var = context.get_variable(id)?;
					let class_id = var.stack_id.class_id();
					let class = self.get_class(class_id)?;
					if let Some(id) = class.function_groups_map.get(*element) {
						item_id = ItemId::FunctionGroup(*id);
						continue;
					} else if class.constants_map.contains_key(*element) {
						todo!()
					}

					return Err(format!(
						"Variable does not have element '{element}'."
					).into());
				},
				ItemId::FunctionGroup(_) => { return Err(format!(
					"Can't index into a function."
				).into()) },
				ItemId::Module(id) => {
					let module = self.get_module(id)?;
					match module.constants_map.get(*element) {
						Some(constant_id) => {
							todo!();
							continue;
						},
						None => {},
					};
					match module.function_groups_map.get(*element) {
						Some(function_group) => {
							item_id = ItemId::FunctionGroup(*function_group);
							continue;
						},
						None => {},
					};
					match module.class_map.get(*element) {
						Some(class_id) => {
							item_id = ItemId::Class(*class_id);
							continue;
						},
						None => {},
					};

					let id = context.find_variable_id(*element)
						.ok_or_else(|| -> QuMsg { format!(
							"Module has no variable named '{}'.", element
						).into() })?;
					item_id = ItemId::Variable(id);
					continue;

					return Err(format!(
						"Module does not have element '{element}'."
					).into());
				},
				ItemId::Function(_) => {todo!()},
				ItemId::ExternalFunction(_) => todo!(),
			}
		}
		Ok(item_id)
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


	pub fn register_module_functions(
		&mut self, module_id: ModuleId,
	) -> Result<(), QuMsg> {
		let mut registrations = vec![];
		for class_id in &self.with_unregistered_functions {
			registrations.push(self.get_class(*class_id)?.register_fn);
		}
		for registration in registrations {
			for external_function in (registration)() {
				self.register_module_function(
					module_id, external_function
				)?;
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
		self.add_class_external_function_map(
			class_id,
			function_identity.clone(),
			new_function_id,
		);

		// Add external function to list
		self.external_functions.push(external_function);

		Ok(())
	}


	pub fn register_module_function(
		&mut self, module_id: ModuleId, external_function:ExternalFunction,
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
		self.add_class_external_function_map(
			class_id, function_identity.clone(), new_function_id,
		);

		// Add identity to module's map
		self.add_module_external_function_map(
			module_id, function_identity, new_function_id,
		);

		// Add external function to list
		self.external_functions.push(external_function);

		Ok(())
	}


	/// Registers an external struct to be used within the Qu langauge.
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
pub struct DotPath<'a> {
	vec: Vec<&'a str>,
}
impl<'a> DotPath<'a> {
	fn add_identity(&mut self, identity: &'a Identity) {
		match identity {
			Identity::Item(item) => {
				self.vec.push(&item.token.slice);
			},
			Identity::Index(index) => {
				self.vec.push(&index.left.token.slice);
				self.add_identity(&index.right);
			},
		}
	}
} impl<'a> From<&'a Identity> for DotPath<'a> {
    fn from(value: &'a Identity) -> Self {
        let mut dotpath = DotPath {..Default::default()};
		dotpath.add_identity(value);
		dotpath
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


#[derive(Debug, Clone)]
struct FrameData {
	scopes: Vec<Scope>,
} impl Default for FrameData {
    fn default() -> Self {
        Self {
			scopes: vec![Scope::default()],
		}
    }
}


pub type FunctionId = usize;


pub type FunctionGroupId = usize;

#[derive(Debug, Default, Clone)]
pub struct FunctionIdentityMap {
	map: HashMap<String, HashMap<ClassId, Vec<FunctionIdentity>>>,
} impl FunctionIdentityMap {
}


pub struct ModuleBuilder<'a>(&'a mut Definitions, ModuleId);
impl<'a> ModuleBuilder<'a> {
	pub fn register_functions(
		&mut self
	) -> Result<(), QuMsg> {
		self.0.register_module_functions(self.1)
	}

	pub fn register_struct<S:QuRegisterStruct+'static>(
		&mut self
	) -> Result<(), QuMsg> {
		self.0.register_module_struct::<S>(self.1)
	}
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


#[derive(Debug, Clone)]
pub enum ItemId {
	Class(ClassId),
	Variable(VariableId),
	Function(FunctionId),
	FunctionGroup(FunctionGroupId),
	ExternalFunction(ExternalFunctionId),
	Module(ModuleId),
} impl Default for ItemId {
	fn default() -> Self {
		Self::Module(0)
	}
} impl From<&ContextFrame> for ItemId {
    fn from(value: &ContextFrame) -> Self {
        match value {
            ContextFrame::Module(id, _) => ItemId::Module(*id),
            ContextFrame::Class(id, _) => ItemId::Class(*id),
            ContextFrame::Function(id, _) => ItemId::Function(*id),
        }
    }
}


#[derive(Debug, Clone)]
/// Reresents a Qu module.
pub struct ModuleMetadata {
	constants_map: HashMap<String, ConstantId>,
	class_map: HashMap<String, ClassId>,
	external_functions_map: HashMap<FunctionIdentity, ExternalFunctionId>,
	functions_map: HashMap<FunctionIdentity, ConstantId>,
	function_groups_map: HashMap<String, FunctionGroupId>,
	static_variables_map: HashMap<String, VariableId>,

	/// The name of this module.
	name: String,
	/// The ID of this module.
	id: ModuleId,
} impl ModuleMetadata {

} impl Default for ModuleMetadata {
	fn default() -> Self {
		Self {
			class_map: Default::default(),
			constants_map : Default::default(),
			name: "__main__".into(),
			id : Default::default(),
			external_functions_map: Default::default(),
			functions_map: Default::default(),
			function_groups_map: Default::default(),
			static_variables_map: Default::default(),
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
} impl FunctionIdentity {
	fn id_self_class(&self) -> ClassId {
		match self.parameters.first() {
			Some(id) => *id,
			None => ClassId(0),
		}
	}
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
	id: FunctionId,
	pub identity: FunctionIdentity,
	/// The value that the VM's program counter should be set to in order to
	/// start this function.
	pub pc_start: usize,
}


#[derive(Debug, Default, Clone)]
struct Scope {
	/// The number of bytes that have been allocated to the stack in this scope.
	stack_size: usize,
	/// The number of bytes that have been defined in this scope.
	variables_map: HashMap<String, VariableId>,
}


pub type SomeFunctionGroup = HashMap<FunctionIdentity, SomeFunctionId>;


#[derive(Debug, Clone)]
pub enum SomeFunctionId {
	Qu(FunctionId),
	External(ExternalFunctionId),
} impl Default for SomeFunctionId {
    fn default() -> Self {
        Self::Qu(0)
    }
}


type VariableId = usize;


#[derive(Debug, Default, Clone)]
struct VariableMetadata {
	name: String,
	stack_id: QuStackId,
}


/// Compiles [QuLeaf]s into Qu bytecode.
#[derive(Debug, Default, Clone)]
pub struct QuCompiler {
	context: Context,
	name_refs: HashMap<String, u32>,
	types_map: HashMap<String, usize>,
} impl QuCompiler {
	/// Creates and returns a new [QuCompiler].
	pub fn new() -> Self {
		let mut inst = Self {
			..Default::default()
		};

		return inst;
	}


	fn class_id_from_call_expression(
		&self,
		call_expression: &CallExpression,
		definitions: &mut Definitions,
	) -> Result<ClassId, QuMsg> {
		// Construct function identity
		let mut parameters = vec![];
		{
			// Make dot indexer the first parameter
			let mut dot_path:DotPath = (&call_expression.name).into();
			if dot_path.vec.len() > 1 {
				let len = dot_path.vec.len()-1;
				let (first_parameter_slice_path, _) = dot_path.vec
					.split_at_mut(len);
				let param_path = DotPath {
					vec: first_parameter_slice_path.into()
				};
				let item = definitions.path_index(
					&param_path, &self.context
				)?;
				if let ItemId::Variable(id) = item {
					let var_stack_id = self.context
						.get_variable(id)?
						.stack_id;
					parameters.push(var_stack_id.class_id());
				}
			}
		}
		for parameter in &call_expression.parameters.elements {
			let object_id = self.get_expr_type(
				parameter,
				definitions,
			)?;
			parameters.push(object_id);
		}
		let function_identity = FunctionIdentity {
			name: call_expression.name.last().token.slice.clone(),
			parameters,
			..Default::default()
		};

		// Get function group
		let item = definitions.path_index(
			&(&call_expression.name).into(),
			&self.context,
		)?;
		let ItemId::FunctionGroup(group_id) = item else {
			return Err(format!(
				"'{}' is not a function.", &call_expression.name,
			).into())
		};
		let group = &definitions
			.function_groups[group_id];
		
		let some_id = group.get(&function_identity)
			.ok_or_else(|| -> QuMsg { format!(
				"No function with that signature. TODO",
			).into() })?;

		// Get return type
		let return_type_id = match some_id {
			SomeFunctionId::Qu(id) => {
				let return_type_id = definitions
					.get_function(*id)?
					.identity
					.return_type;
				return_type_id
			},
			SomeFunctionId::External(id) => {
				let return_type_id = definitions
					.get_external_function(*id)?
					.return_type;
				let return_type_id = definitions
					.find_class_id(return_type_id)?;
				return_type_id
			},
		};

		Ok(return_type_id)
	}


	fn class_id_from_option_identity(
		&self,
		option_identity: &Option<Identity>,
		definitions: &mut Definitions,
	) -> Result<ClassId, QuMsg> {
		let class_id = match &option_identity {
			Some(identity) => {
				let item_id = &definitions.path_index(
					&identity.into(),
					&self.context,
				)?;
				let ItemId::Class(class_id) = item_id
				else { return Err(format!(
					"'{}' is not a class.", identity,
				).into()) };
				*class_id
			},
			None => {
				definitions.class_id::<QuVoid>()?
			},
		};
		Ok(class_id)
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
					&call_expression,
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
			Expression::Var(var_expression)
				=> self.cmp_expr_var(
					&var_expression,
					output_reg,
					definitions,
				),
		}?;

		// Type check
		let void_id = definitions.class_id::<QuVoid>()?;
		if builder.return_type != void_id {
			if builder.return_type != output_reg.class_id() {
				return Err(format!(
					"Attempted to assign a value of type '{}' to a location of type '{}'.",
					definitions.get_class(builder.return_type)?.name,
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
		// TODO: reimplment without copying
		let call_expression = CallExpression {
			name: QuOperator::from(operator.slice.as_str()).name().into(),
			parameters: TupleExpression {elements: vec![left.clone(), right.clone()]},
			..Default::default()
		};
		self.cmp_fn_call(
			&call_expression,
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
		b.return_type = definitions.class_id::<i32>()?;

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
	fn cmp_expr_var(
		&mut self,
		var_expression: &VarExpression,
		output_reg: QuStackId,
		definitions: &mut Definitions,
	) -> Result<QuAsmBuilder, QuMsg> {

		// TODO: Error handling
		
		let ItemId::Variable(var_id) = definitions.path_index(
			&(&var_expression.ident).into(),
			&self.context
		)? else { return Err(format!(
			"'{}' is not a variable.", var_expression.ident,
		).into()); };
		let variable = self.context
			.get_variable(var_id)?;
		
		if variable.stack_id == output_reg {
			// Output and variable locations are the same, do nothing
			let mut b = QuAsmBuilder::default();
			b.return_type = variable.stack_id.class_id();
			return Ok(b);
		}

		let identity = &FunctionIdentity {
			name: "copy".into(),
			parameters: vec![variable.stack_id.class_id()],
			return_type: variable.stack_id.class_id(),
		};

		let mut b = QuAsmBuilder::new();
		b.add_op(QuOp::CallExt(
			definitions.get_external_function_id_by_identity(identity)?,
			vec![variable.stack_id],
			output_reg,
		));

		b.return_type = variable.stack_id.class_id();

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
		self.context.open_scope();
		let mut expr_b = {
			let expr_reg = self.get_expr_reg(&mut condition, definitions)?;
			self.cmp_expr(
				condition,
				QuStackId::new(
					expr_reg.index(),
					definitions.class_id::<bool>()?,
				),
				definitions
			)
		}?;
		self.context.close_scope();
	
		// New frame for the code in the 'if' body
		self.context.open_scope();
		let b:Result<QuAsmBuilder, QuMsg> = {
			let body_code = self.cmp_scope(body, definitions)?;

			let mut b = QuAsmBuilder::new();
			b.add_op(JumpByIfNot(body_code.len() as isize));
			b.add_builder(body_code);

			Ok(b)
		};
		self.context.close_scope();

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
		self.context.open_scope();
		let expr_code = {
			let if_expr_reg = self.get_expr_reg(
				&mut condition,
				definitions,
			)?;
			// Expression code
			self.cmp_expr(
				condition,
				QuStackId::new(
					if_expr_reg.index(),
					definitions.class_id::<bool>()?,
				),
				definitions
			)
		}?;
		self.context.close_scope();

		// New frame for the code in the 'if' body
		self.context.open_scope();
		let code= {
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
		}?;
		self.context.close_scope();

		Ok(code)
	}


	fn cmp_fn_call(
		&mut self,
		call_expression: &CallExpression,
		store_to: QuStackId,
		definitions: &mut Definitions,
	) -> Result<QuAsmBuilder, QuMsg> {
		let mut builder = QuAsmBuilder::new();

		let name = call_expression.name.last().token.slice.clone();
		let mut function_identity = FunctionIdentity {
			name,
			return_type: store_to.class_id(),
			..Default::default()
		};

		// Compile parameter expressions
		let mut parameter_ids = vec![];
		self.context.open_scope();
		{
			let mut dot_path:DotPath = (&call_expression.name).into();
			if dot_path.vec.len() > 1 {
				let len = dot_path.vec.len()-1;
				let (first_parameter_slice_path, _) = dot_path.vec
					.split_at_mut(len);
				let param_path = DotPath {
					vec: first_parameter_slice_path.into()
				};
				let item = definitions.path_index(
					&param_path, &self.context
				)?;
				if let ItemId::Variable(id) = item {
					let var_stack_id = self.context
						.get_variable(id)?
						.stack_id;
					let parameter_stack_id = self.context.allocate(
						var_stack_id.class_id(),
						definitions,
					)?;

					builder.add_op(QuOp::CallExt(
						definitions.get_external_function_id_by_identity(
							&FunctionIdentity {
								name: "copy".into(),
								parameters: vec![var_stack_id.class_id()],
								return_type: var_stack_id.class_id(),
							}
						)?,
						vec![var_stack_id],
						parameter_stack_id,
					));

					function_identity.parameters.push(var_stack_id.class_id());
					parameter_ids.push(var_stack_id);
				}
			}

			for parameter in &call_expression.parameters.elements {
				let object_id = self.get_expr_type(parameter, definitions)?;
				let var_stack_id = self.context.allocate(
					object_id,
					definitions,
				)?;
				let parameter_expr = self.cmp_expr(
					parameter,
					var_stack_id,
					definitions,
				)?;
				function_identity.parameters.push(object_id);
				builder.add_builder(parameter_expr);
				parameter_ids.push(var_stack_id);
			}
			Ok::<(), QuMsg>(())
		}?;
		self.context.close_scope();

		// Find function id by the current context
		use ItemId::FunctionGroup;
		let FunctionGroup(group_id) = definitions
			.path_index(&(&call_expression.name).into(), &self.context,)?
		else { return Err(format!(
			"'{}' is not a function.", call_expression.name
		).into()) };
		let group = &definitions
			.function_groups[group_id];
		let id = group
			.get(&function_identity)
			.unwrap();

		// Compile the function call
		match id {
			SomeFunctionId::Qu(function_id) => {
				builder.add_op(QuOp::Call(*function_id, store_to));
			},
			SomeFunctionId::External(external_function_id) => {
				builder.add_op(QuOp::CallExt(
					*external_function_id,
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
		parsed_identity: &crate::parser::parsed::FunctionIdentity,
		body: &CodeScope,
		definitions: &mut Definitions,
	) -> Result<QuAsmBuilder, QuMsg> {
		let needs_added_end_op = match body.code_block.statements.last() {
			Some(Statement::Return(_)) => false,
			_ => true,
		};

		let (identity, parameters) = {
			let mut parameters_types = vec![];
			let mut parameters_names = vec![];
			for param in &parsed_identity.parameters {
				match &param.static_type {
					Some(identity) => {
						let item_id = &definitions.path_index(
							&identity.into(),
							&self.context,
						)?;
						let ItemId::Class(id) = item_id else {panic!()};
						parameters_types.push(*id);
						parameters_names.push((param.name().to_owned(), *id))
					},
					None => {
						let id = definitions.class_id::<QuVoid>()?;
						parameters_types.push(id);
						parameters_names.push((param.name().to_owned(), id))
					},
				};
			}
			let return_type = self.class_id_from_option_identity(
				&parsed_identity.return_type,
				definitions,
			)?;
			let identity = FunctionIdentity {
				name: parsed_identity.name.last().token.slice.clone(),
				parameters: parameters_types,
				return_type,
			};
			(identity, parameters_names)
		};
		
		let func_id = definitions
			.get_function_id_by_identity(&identity)?;

		self.context.open_frame(ContextFrame::Function(
			func_id, FrameData::default(),
		));
		let body_code = {
			// Allocate return value
			self.context.define_variable(
				"return value".into(),
				identity.return_type,
				definitions,
			)?;

			// Allocate parameters
			for (name, class_id) in &parameters {
				self.context.define_variable(
					name.into(),
					*class_id,
					definitions,
				)?;
			}

			// Compile code block
			let mut b = QuAsmBuilder::new();
			b.add_builder(self.cmp_code_block(&body.code_block, definitions)?);
			if needs_added_end_op {
				b.add_op(End);
			}
			Ok::<QuAsmBuilder, QuMsg>(b)
		}?;
		self.context.close_frame();

		// Add fn declaration operation
		let mut code = QuAsmBuilder::new();
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
				self.context.open_scope();
				let result = {
					let reg = self.get_expr_reg(
						&expression,
						definitions,
					)?;
					self.cmp_expr(expression, reg, definitions)
				};
				self.context.close_scope();
				return result;
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
						let expression_reg = self.context.allocate(
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
							expression_reg,
							definitions
						)?);
						code.add_op(QuOp::CallExt(
							definitions.get_external_function_id_by_identity(&copy_idenity)?,
							vec![expression_reg],
							return_reg,
						));

						if self.context.frames.len() == 1 {
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
					&var_declaration,
					definitions
				);
			}
			Statement::VarAssign(variable_assignment) => {
				return  self.cmp_var_assign(
					variable_assignment,
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
		var_assignment: &VarAssignment,
		definitions: &mut Definitions,
	) -> Result<QuAsmBuilder, QuMsg> {
		let item = definitions.path_index(
			&(&var_assignment.name).into(),
			&self.context,
		)?;
		let ItemId::Variable(id) = item else { return Err(format!(
			"'{}' is not a variable.", var_assignment.name
		).into()) };
		let variable = self.context.get_variable(id)?;
		// Compile assignment to expression
		return self.cmp_expr(
			&var_assignment.new_value,
			variable.stack_id,
			definitions,
		);
	}


	/// Compiles a variable declaration.
	fn cmp_var_decl(
		&mut self,
		var_declaration: &VarDeclaration,
		definitions: &mut Definitions,
	) -> Result<QuAsmBuilder, QuMsg> {
		let ident = &var_declaration.name;

		// Check if the variable is already defined
		if definitions.has_path_index(&ident.into(), &self.context)? {
			return Err(format!(
				"An item by '{}' is already defined.", ident,
			).into());
		}

		let static_type = self.class_id_from_option_identity(
			&var_declaration.static_type,
			definitions
		)?;
		let var_stack_id = self.context.define_variable(
			ident.last().token.slice.clone(),
			static_type,
			definitions
		)?.stack_id;

		// Compile variable assignment
		return match &var_declaration.initial_value {
			// Compile variable value
			Some(expression)
				=> self.cmp_expr(
					expression,
					var_stack_id,
					definitions,
				),

			None => {
				// No default value, compile fallback to zero
				let mut code = QuAsmBuilder::new();
				code.add_op(Value(0, var_stack_id));
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
		self.context.open_scope();
		let compiled= self.cmp_code_block(
			&code_scope.code_block, definitions,
		);
		self.context.close_scope();
		return compiled;
	}


	/// Compiles Qu code from a [`&str`] into a [`Vec<u8>`].
	pub fn compile(
		&mut self, code:&str, definitions: &mut Definitions,
	) -> Result<Vec<QuOp>, QuMsg> {
		let mut p = QuParser::new();
		let code_block = p.parse(code)?;

		let main_id = *definitions.module_map
			.get("__main__")
			.unwrap();
		
		self.context.open_frame(ContextFrame::module(main_id));
		self.prepass(&code_block, definitions)?;
		let compiled = self.compile_code(&code_block, definitions)?;
		self.context.close_frame();

		Ok((compiled))
	}


	/// Compiles Qu code from a [QuLeaf] into a [`Vec<u8>`].
	pub fn compile_code(
		&mut self, code_block:&CodeBlock, definitions: &mut Definitions
	) -> Result<Vec<QuOp>, QuMsg> {
		// Main code
		let mut code = {
			let mut code = self.cmp_code_block(code_block, definitions)?;
			code.add_op(End);
			Ok::<QuAsmBuilder, QuMsg>(code)
		}?;
		
		Ok(code.compile(
			&self.name_refs,
			definitions,
		)?)
	}


	/// Returns an appropriate location to store an expression.
	/// 
	/// Most expressions require a new memory location, but variables
	/// should just return the register of the variable.
	fn get_expr_reg(
		&mut self, expr_leaf:&Expression, definitions: &mut Definitions
	) -> Result<QuStackId, QuMsg> {
		return  match expr_leaf {
			Expression::Operation(operation) => {
				// TODO: reimplement without cloning
				let call_expression = CallExpression {
					name: QuOperator::from(
						operation.operator.slice.as_str()
					).name().into(),
					parameters: TupleExpression {
						elements: vec![operation.left.clone(), operation.right.clone()]
					},
					..Default::default()
				};
				self.get_expr_reg(
					&Expression::Call(Box::new(call_expression)),
					definitions,
				)
			},
			Expression::Call(call) => {
				let class_id = self.class_id_from_call_expression(
					call, definitions,
				)?;
				self.context.allocate(class_id, definitions)
			},
			Expression::Number(_) => {
				self.context.allocate(
					definitions.class_id::<i32>()?, definitions
				)
			},
			Expression::Tuple(_) => panic!(),
			Expression::Var(var) => {
				let item = definitions.path_index(
					&(&var.ident).into(),
					&self.context,
				)?;
				let ItemId::Variable(id) = item else { return Err(format!(
					"'{}' is not a variable", var.ident,
				).into()) };
				let var = self.context.get_variable(id)?;
				Ok(var.stack_id)
			}
		};
	}


	/// Returns the static type of an expression.
	fn get_expr_type(
		&self, expr_leaf:&Expression, definitions: &mut Definitions
	) -> Result<ClassId, QuMsg> {
		let object_id = match expr_leaf {
			Expression::Operation(operation) => {
				// TODO: Reimplement without copying
				self.class_id_from_call_expression(
					&(**operation).clone().into(),
					definitions,
				)?
			},
			Expression::Call(call_expression) => {
				self.class_id_from_call_expression(
					&call_expression,
					definitions,
				)?
			},
			Expression::Number(_) => {
				definitions.class_id::<i32>()?
			},
			Expression::Tuple(tuple) => {
				unimplemented!()
			},
			Expression::Var(var) => {
				let item = definitions.path_index(
					&(&var.ident).into(),
					&self.context,
				)?;
				let ItemId::Variable(id) = item else { return Err(format!(
					"'{}' is not a variable.", &var.ident,
				).into()) };
				let var = self.context.get_variable(id)?;
				var.stack_id.class_id()
			}
		};
		Ok(object_id)
	}


	fn prepass(
		&mut self, code_block:&CodeBlock, definitions:&mut Definitions,
	) -> Result<(), QuMsg> {
		let module_id = if !definitions.module_map.contains_key("__main__") {
			definitions.define_module(
				"__main__".into(),
				&|_| {Ok(())},
			)?
		} else {
			definitions.get_module_id("__main__")?
		};

		// Function definitions
		for statement in &code_block.statements {
			match statement {
				Statement::FunctionDeclaration(function_declaration) => {
					let signature = &function_declaration
						.identity;

					// Make function identity
					let mut parameters = vec![];
					for parameter in &signature.parameters {
						parameters.push(self.class_id_from_option_identity(
							&parameter.static_type,
							definitions,
						)?);
					}
					let return_type = self.class_id_from_option_identity(
						&signature.return_type, definitions,
					)?;
					let function_identity = FunctionIdentity {
						name: signature.name.last().token.slice.clone(),
						parameters,
						return_type,
					};

					// Add function to definitions
					definitions.define_module_function(
						module_id,
						function_identity,
					)?;
				},
				_ => {},
			}
		}

		Ok(())
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
	code_pieces: Vec<QuBuilderPiece>,
	return_type: ClassId,
} impl QuAsmBuilder {

	fn new() -> Self {
		return Self {
			code_pieces: vec![],
			return_type: ClassId::new(0),
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
