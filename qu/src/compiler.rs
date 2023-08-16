
use crate::Class;
use crate::ExternalFunction;
use crate::ExternalFunctionDefinition;
use crate::Module;
use crate::QuOp;
use crate::QuOp::*;
use crate::QuParser;
use crate::QuRegisterStruct;
use crate::QuStackId;
use crate::QuVoid;
use crate::import::QuRegistered;
use crate::import::QuStruct;
use crate::import::ClassId;
use crate::objects::Bool;
use crate::parser::KEYWORD_IF;
use crate::parser::QuOperator;
use crate::parser::parsed::*;

use crate::QuMsg;
use crate::QuToken;
use crate::vm::FUNDAMENTALS_MODULE;
use crate::vm::MAIN_MODULE;
use crate::vm::Stack;

use core::panic;
use std::collections::HashMap;
use std::fmt::Display;
use std::mem::size_of;
use std::hash::Hash;
use std::mem::take;

// TODO: Fix compiler's documentation

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
			constants_map: Default::default(),
			external_functions_map: Default::default(),
			functions_map: Default::default(),
			id: Default::default(),
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
		match self.get_current_context_frame_mut() {
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
				scope.define_variable(name.clone(), self.variables.len())?;

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
				scope.define_variable(name.clone(), self.variables.len())?;

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
		let scope = frame.scopes.pop().unwrap();
	}


	fn find_item(
		&self,
		identity: &str,
		definitions: &Definitions,
	) -> Result<ItemId, QuMsg> {
		self.find_item_filtered(identity, &|_| {true}, definitions)
	}


	fn find_item_filtered(
		&self,
		identity: &str,
		filter: &dyn Fn(ItemId) -> bool,
		definitions: &Definitions,
	) -> Result<ItemId, QuMsg> {
		let item = 'scopes: {
			for frame in self.frames.iter().rev() {
				let frame_data = frame.get_frame();
				for scope in frame_data.scopes.iter().rev() {
					let item_opt = scope
						.find_item(identity);
					if let Some(item) = item_opt {
						if filter(item) {
							break 'scopes Some(item);
						}
					}
				}
			}
			None
		};
		if let Some(item) = item {
			return Ok(item)
		}
		

		let item = 'frames: {
			for frame in self.frames.iter().rev() {
				match frame {
					ContextFrame::Module(module_id, _) => {
						let module = definitions.get_module(
							*module_id
						)?;
						let item_opt = if
							!module.has_item(identity)
						{
							None
						} else {
							Some(module.get_item_id(identity)?)
						};

						if let Some(item) = item_opt {
							if filter(item) {
								break 'frames Some(item);
							}
						}
					},
					ContextFrame::Class(class_id, _) => {
						let class = definitions
							.get_class(*class_id)?;
						let item_opt = if
							!class.has_item(identity)
						{
							None
						} else {
							Some(class.get_item_id(identity)?)
						};

						if let Some(item) = item_opt {
							if filter(item) {
								break 'frames Some(item);
							}
						}
					},
					ContextFrame::Function(function_id, _) => {
						continue;
					},
				}
			}

			None
		};

		let Some(item) = item else {
			return Err(format!(
				"Could not find anything with name '{}' in current context",
				identity,
			).into());
		};

		Ok(item)
	}

	fn get_current_context_frame(&self) -> &ContextFrame {
		self.frames
			.last()
			.unwrap()
	}


	fn get_current_context_frame_mut(&mut self) -> &mut ContextFrame {
		self.frames
			.last_mut()
			.unwrap()
	}


	pub fn get_some_function_id_by_identity(
		&self,
		identity: &FunctionIdentity,
		definitions: &Definitions,
	) -> Result<SomeFunctionId, QuMsg> {
		let item = self.find_item_filtered(
			&identity.name,
			&|item| {
				let ItemId::FunctionGroup(id) = item else {
					return false;
				};
				let group = definitions
					.get_function_group(id)
					.unwrap();
				group.map.contains_key(identity)
			},
			definitions,
		)?;
		let ItemId::FunctionGroup(function_group) = item else {
			return Err(format!(
				"error in Context::get_some_function_id_by_identity TODO",
			).into());
		};

		let group = &definitions
			.function_groups[function_group];
		let some_fn_id = group.map[identity];

		Ok(some_fn_id)
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


	fn has_item(
		&self,
		identity: &str,
		definitions: &Definitions,
	) -> bool {
		let scope = self.get_current_context_frame()
			.get_frame()
			.get_current_scope();
		if let Some(_) = scope.find_item(identity) {
			return true;
		}

		let has_item = 'frames: {
			for frame in self.frames.iter().rev() {
				match frame {
					ContextFrame::Module(module_id, _) => {
						let Ok(module) = definitions
							.get_module(*module_id) else { return false };
						break 'frames module.has_item(identity);
					},
					ContextFrame::Class(class_id, _) => {
						let Ok(class) = definitions
							.get_class(*class_id) else { return false };
						break 'frames class.has_item(identity);
					},
					ContextFrame::Function(_, _) => {
						continue;
					},
				}
			}

			false
		};

		has_item
	}


	fn import_class(
		&mut self,
		id: ClassId,
		alias: Option<String>,
		definitions: &Definitions,
	) -> Result<(), QuMsg> {
		self.frames
			.last_mut()
			.unwrap()
			.get_frame_mut();
		let scope = self.get_current_context_frame_mut()
			.get_frame_mut()
			.get_current_scope_mut();
		scope.import_class(id, alias, definitions)?;

		Ok(())
	}


	fn import_function(
		&mut self,
		id: FunctionGroupId,
		alias: Option<String>,
		definitions: &Definitions,
	) -> Result<(), QuMsg> {
		self.frames
			.last_mut()
			.unwrap()
			.get_frame_mut();
		let scope = self.get_current_context_frame_mut()
			.get_frame_mut()
			.get_current_scope_mut();
		scope.import_function(id, alias, definitions)?;

		Ok(())
	}


	fn import_module(
		&mut self,
		module_id: ModuleId,
		alias: Option<String>,
		definitions: &Definitions,
	) -> Result<(), QuMsg> {
		self.frames
			.last_mut()
			.unwrap()
			.get_frame_mut();
		let scope = self.get_current_context_frame_mut()
			.get_frame_mut()
			.get_current_scope_mut();
		scope.import_module(module_id, alias, definitions)?;

		Ok(())
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


	fn get_frame(&self) -> &FrameData {
		match self {
			ContextFrame::Module(_, frame_data) => frame_data,
			ContextFrame::Class(_, frame_data) => frame_data,
			ContextFrame::Function(_, frame_data) => frame_data,
		}
	}


	fn get_frame_mut(&mut self) -> &mut FrameData {
		match self {
			ContextFrame::Module(_, frame_data) => frame_data,
			ContextFrame::Class(_, frame_data) => frame_data,
			ContextFrame::Function(_, frame_data) => frame_data,
		}
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
	pub external_functions: Vec<ExternalFunctionDefinition>,
	pub functions: Vec<FunctionMetadata>,
	pub function_groups: Vec<FunctionGroup>,
	pub modules: Vec<ModuleMetadata>,

	pub byte_code_blocks: Vec<Vec<QuOp>>,

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
		module_id: ModuleId,
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
				let module = self.get_module_mut(module_id)?;
				let id = match &module.function_groups_map.get(name) {
			Some(id) => {**id},
			None => {
				let id = new_id;
						module.function_groups_map.insert(
							name.into(), id,
						);
						let mut group = FunctionGroup::default();
						group.name = name.clone();
						self.function_groups.push(group);
						id
					}
				};
				let class = self.get_class_mut(class_id)?;
				class.function_groups_map.insert(
					name.into(), id,
				);
				let mut group = FunctionGroup::default();
				group.name = name.clone();
				self.function_groups.push(group);
				id
			}
		};
		

		let group = &mut self
			.function_groups[group_id];
		if group.map.contains_key(&signature) {
			// The function couldn't be registered because a function with
			// its signature already exists.
			let module_name = &self.get_module(module_id).unwrap().name;
			return Err(format!(
				"Couldn't register function '{signature}' in module '{module_name}'. A function with that signature already exists."
			).into())
		}
		group.map.insert(signature.clone(), SomeFunctionId::External(id)); // TODO: Remove cloning
		
		// Add to class mappings
		self.get_class_mut(class_id)?
			.external_functions_map
			.insert(signature.clone(), id); // TODO: Remove cloning

		// Add to module mappings
		self.get_module_mut(module_id)?
			.external_functions_map
			.insert(signature, id);

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
				let mut group = FunctionGroup::default();
				group.name = name.clone();
				self.function_groups.push(group);
				id
			}
		};
		let group = &mut self
			.function_groups[group_id];

		// Add to mappings
		assert!(!group.map.contains_key(&signature));
		group.map.insert(signature.clone(), SomeFunctionId::Qu(id)); // TODO: Remove cloning
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
				let mut group = FunctionGroup::default();
				group.name = name.clone();
				self.function_groups.push(group);
				id
			}
		};
		let group = &mut self
			.function_groups[group_id];

		// Add to mappings
		assert!(!group.map.contains_key(&signature));
		group.map.insert(signature.clone(), SomeFunctionId::External(id)); // TODO: Remove cloning
		self.get_module_mut(module_id)?
			.external_functions_map
			.insert(signature, id);
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
				let mut group = FunctionGroup::default();
				group.name = name.clone();
				self.function_groups.push(group);
				id
			}
		};
		let group = &mut self
			.function_groups[group_id];

		// Add to module's group mappings
		assert!(!group.map.contains_key(&signature));
		group.map.insert(signature.clone(), SomeFunctionId::Qu(id)); // TODO: Remove cloning
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
	) -> Result<&ExternalFunctionDefinition, QuMsg> {
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


	pub fn get_function_group(
		&self,
		id: FunctionGroupId,
	) -> Result<&FunctionGroup, QuMsg> {
		let group = self.function_groups
			.get(usize::from(id))
			.ok_or_else(|| -> QuMsg { format!(
				"There's no function with id '{:?}' defined.", id,
			).into()})?;
		Ok(group)
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


	pub fn get_module_by_name(&self, module:&str) -> Result<&ModuleMetadata, QuMsg> {
		let id = self.get_module_id(module).ok_or_else(|| {
			QuMsg::from(format!("Found no module by name {module}"))
		})?;
		self.get_module(id)
	}


	pub fn get_module_id(&self, module:&str) -> Option<ModuleId> {
		if !self.module_map.contains_key(module) {
			return None;
		}
		self.module_map.get(module).map(|x| {*x})
	}


	pub fn get_some_function_id_by_identity(
		&self,
		identity: &FunctionIdentity,
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
			for external_function in (registration)(self) {
				self.register_function(external_function)?;
			}
		}
		self.with_unregistered_functions.clear();
		Ok(())
	}


	pub fn register_module_functions(
		&mut self,
		module_id: ModuleId,
	) -> Result<(), QuMsg> {
		let mut registrations = vec![];
		for class_id in &self.with_unregistered_functions {
			registrations.push(self.get_class(*class_id)?.register_fn);
		}
		for registration in registrations {
			for external_function in (registration)(self) {
				self.register_module_function(
					module_id,
					external_function,
				)?;
			}
		}
		self.with_unregistered_functions.clear();
		Ok(())
	}


	/// Registers an [`ExternalFunction`] to Qu.
	pub fn register_function(
		&mut self,
		external_function:ExternalFunctionDefinition,
	) -> Result<(), QuMsg> {
		// Make function identity
		let mut parameters = vec![];
		for parameter in &external_function.parameters {
			let class_id = parameter;
			parameters.push(class_id);
		}
		let function_identity = FunctionIdentity {
			name: external_function.name.clone(),
			parameters: external_function.parameters.clone(),
			return_type: external_function.return_type,
		};

		// Get class id of first parameter
		let class_id = match function_identity.parameters.first() {
			Some(class_id) => *class_id,
			None => {todo!()},
		};

		// Add identity to class's map
		let new_function_id = self.external_functions.len();
		// self.add_class_external_function_map(
		// 	class_id,
		// 	function_identity.clone(),
		// 	new_function_id,
		// );

		// Add external function to list
		self.external_functions.push(external_function);

		Ok(())
	}


	pub fn register_module_function(
		&mut self,
		module_id: ModuleId,
		external_function:ExternalFunctionDefinition,
	) -> Result<(), QuMsg> {
		// Make function identity
		let function_identity = FunctionIdentity {
			name: external_function.name.clone(),
			parameters: external_function.parameters.clone(),
			return_type: external_function.return_type,
		};

		// Get class id of first parameter
		let class_id = match function_identity.parameters.first() {
			Some(class_id) => *class_id,
			None => {todo!()},
		};

		// Add identity to class's and module's map
		let new_function_id = self.external_functions.len();
		self.add_class_external_function_map(
			class_id,
			module_id,
			function_identity.clone(),
			new_function_id,
		).unwrap();

		// Add external function to list
		self.external_functions.push(external_function);

		Ok(())
	}


	/// Registers an external struct to be used within the Qu langauge.
	pub fn register_module_struct<S:QuRegisterStruct+'static>(
		&mut self,
		module_id: ModuleId,
	) -> Result<(), QuMsg> {
		let class_name = <S as QuRegisterStruct>::name();

		// Manage classes map
		let class_id:ClassId = self.classes.len().into();
		let module = self.get_module_mut(module_id)?;
		assert!(!module.class_map.contains_key(class_name));
		module.class_map.insert(class_name.into(), class_id);
		self.with_unregistered_functions.push(class_id);

		// Add class to list of classes
		let external_function_definitions = &<S as QuRegisterStruct>::register_fns;
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


#[derive(Debug, Clone)]
struct FrameData {
	scopes: Vec<Scope>,
} impl FrameData {
	fn get_current_scope(&self) -> &Scope {
		self.scopes.last().unwrap()
	}


	fn get_current_scope_mut(&mut self) -> &mut Scope {
		self.scopes.last_mut().unwrap()
	}
} impl Default for FrameData {
	fn default() -> Self {
		Self {
			scopes: vec![Scope::default()],
		}
	}
}


#[derive(Debug, Default, Clone)]
pub struct FunctionGroup {
	map: HashMap<FunctionIdentity, SomeFunctionId>,
	name: String,
}

pub type FunctionGroupId = usize;

pub type FunctionId = usize;


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
} impl Display for FunctionIdentity {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		let mut params = String::new();
		for p in &self.parameters {
			let id = p.0;
			params.push_str(&format!("type:{id}"));
		}
		write!(f,"{}({}) type:{}", self.name, params, self.return_type.0)
	}
}


#[derive(Debug, Default, Clone)]
pub struct FunctionMetadata {
	id: FunctionId,
	pub identity: FunctionIdentity,
	/// The value that the VM's program counter should be set to in order to
	/// start this function.
	pub code_block: usize,
}


#[derive(Debug, Default, Clone)]
pub struct FunctionIdentityMap {
	map: HashMap<String, HashMap<ClassId, Vec<FunctionIdentity>>>,
} impl FunctionIdentityMap {
}


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
	fn has_item(&self, identity: &str) -> bool {
		self.constants_map.contains_key(identity)
			|| self.class_map.contains_key(identity)
			|| self.function_groups_map.contains_key(identity)
			|| self.static_variables_map.contains_key(identity)
	}
	

	pub fn get_class_id(
		&self,
		class_name: &str,
	) -> Result<ClassId, QuMsg> {
		self.class_map
			.get(class_name)
			.ok_or_else(|| -> QuMsg {
				format!(
					"Found no class named '{}' in module '{}'",
					class_name,
					self.name,
				).into()
			})
			.map(|id| {*id})
	}

	
	pub fn get_function_group_id(
		&self,
		function_name: &str,
	) -> Result<FunctionGroupId, QuMsg> {
		self.function_groups_map
			.get(function_name)
			.ok_or_else(||->QuMsg {
				format!(
					"Module '{}' has no function named '{}'",
					self.name,
					function_name
				).into()
			})
			.map(|id| {*id})
	}


	fn get_item_id(&self, identity: &str) -> Result<ItemId, QuMsg> {
		if let Some(id) = self.constants_map.get(identity) {
			return Ok(ItemId::Constant(*id));
		}
		if let Some(id) = self.class_map.get(identity) {
			return Ok(ItemId::Class(*id));
		}
		if let Some(id) = self.function_groups_map.get(identity) {
			return Ok(ItemId::FunctionGroup(*id));
		}
		if let Some(id) = self.static_variables_map.get(identity) {
			return Ok(ItemId::StaticVariable(*id));
		}

		Err(format!(
			"Modules '{}' has no item by name '{}'.", self.name, identity,
		).into())
	}
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



#[derive(Debug, Clone, Copy)]
pub enum ItemId {
	Class(ClassId),
	Constant(ConstantId),
	ExternalFunction(ExternalFunctionId),
	Function(FunctionId),
	FunctionGroup(FunctionGroupId),
	Module(ModuleId),
	StaticVariable(VariableId),
	Variable(VariableId),
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

#[derive(Debug, Default, Clone)]
struct Scope {
	/// A map of everything defined in this scope.
	definitions_map: HashMap<String, ItemId>,
	function_groups_map: HashMap<String, FunctionGroupId>,
	/// The number of bytes that have been allocated to the stack in this scope.
	stack_size: usize,
} impl Scope {
	fn define_variable(
		&mut self,
		name: String,
		id: VariableId,
	) -> Result<(), QuMsg> {
		if self.definitions_map.contains_key(&name) {
			return Err(format!(
				"Variable with name `{}` is already defined in the current scope.",
				&name,
			).into());
		}

		self.definitions_map.insert(
			name,
			ItemId::Variable(id),
		);

		Ok(())
	}


	fn find_item(
		&self,
		name: &str,
	) -> Option<ItemId> {
		let Some(item) = self.definitions_map.get(name) else {
			return None
		};
		Some(item.clone())
	}


	fn import_class(
		&mut self,
		id: ClassId,
		alias: Option<String>,
		definitions: &Definitions
	) -> Result<(), QuMsg>{
		let name = match alias {
			Some(name) => name,
			None => {
				let data = definitions
					.get_class(id)?;
				data.name.clone()
			},
		};

		self.definitions_map.insert(
			name,
			ItemId::Class(id),
		);

		Ok(())
	}


	fn import_function(
		&mut self,
		id: FunctionGroupId,
		alias: Option<String>,
		definitions: &Definitions
	) -> Result<(), QuMsg>{
		let name = match alias {
			Some(name) => name,
			None => {
				let group = definitions
					.get_function_group(id)?;
				group.name.clone()
			},
		};

		self.definitions_map.insert(
			name,
			ItemId::FunctionGroup(id),
		);

		Ok(())
	}


	fn import_module(
		&mut self,
		module_id: ModuleId,
		alias: Option<String>,
		definitions: &Definitions
	) -> Result<(), QuMsg>{
		let name = match alias {
			Some(name) => name,
			None => {
				let module = definitions
					.get_module(module_id)?;
				module.name.clone()
			},
		};

		self.definitions_map.insert(
			name,
			ItemId::Module(module_id),
		);

		Ok(())
	}
}


#[derive(Debug, Clone, Copy)]
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
		let mut caller_id = match &call_expression.caller {
			Some(caller_expression) => {
				// Caller is inferred by dot notation
				self.get_expr_type(caller_expression, definitions)?
			},
			None => {
				if call_expression.parameters.len() != 0 {
					// Caller is inferred by first parameter
					self.get_expr_type(
						&call_expression.parameters.elements[0],
						definitions,
					)?
				} else {
					// Function has no parameters
					definitions.class_id::<QuVoid>()?
				}
			},
		};
		
		call_expression.caller.as_ref().and_then(|caller|{
			self.get_expr_type(&caller, definitions).ok()
		});

		let parameters = {
			let mut parameters = vec![];
			// Make dot caller the first parameter
			if let Some(_) = &call_expression.caller {
				if caller_id != definitions.class_id::<Module>()?
					&& caller_id != definitions.class_id::<Class>()?
				{
					parameters.push(caller_id);
				}
			}
			// Add other parameters
			for parameter in &call_expression.parameters.elements {
				let object_id = self.get_expr_type(
					parameter,
					definitions,
				)?;
				parameters.push(object_id);
			}
			parameters
		};
		if caller_id == definitions.class_id::<Module>()?
			|| caller_id == definitions.class_id::<Class>()?
		{
			caller_id = *parameters
				.first()
				.unwrap_or(&definitions.class_id::<QuVoid>()?);
		}
		
		// Construct function identity
		let function_identity = FunctionIdentity {
			name: call_expression.name.slice.clone(),
			parameters,
			..Default::default()
		};

		// Get function group
		let function_group = definitions.get_class(caller_id)?
			.get_function_group_id(&call_expression.name.slice)?;
		let group = &definitions
			.function_groups[function_group];
		
		let some_id = group.map.get(&function_identity)
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
				definitions
					.get_external_function(*id)?
					.return_type
			},
		};

		Ok(return_type_id)
	}


	fn class_id_from_option_identity(
		&self,
		option_identity: &Option<QuToken>,
		definitions: &mut Definitions,
	) -> Result<ClassId, QuMsg> {
		let Some(identity) = option_identity else {
			return definitions.class_id::<QuVoid>();
		};
		let class_id = definitions.find_class_id(
			&identity.slice
		)?;
		
		Ok(class_id)
	}


	/// Compiles a copy from one register to another.
	fn cmp_copy_register(
		&self,
		from: QuStackId,
		to: QuStackId,
		definitions: &mut Definitions
	) -> Result<QuAsmBuilder, QuMsg>{
		let identity = &FunctionIdentity {
			name: "copy".into(),
			parameters: vec![from.class_id()],
			return_type: to.class_id(),
		};
		let mut b = QuAsmBuilder::new();
		b.add_op(QuOp::CallExt(
			definitions.get_external_function_id_by_identity(identity)?,
			vec![from],
			to,
		));
		b.return_type = to.class_id();

		Ok(b)
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
			Expression::DotIndex(
				dot_index,
			) => self.cmp_expr_dot_index(
				dot_index,
				output_reg,
				definitions,
			),
			Expression::Operation(
				operation_expression,
			) => self.cmp_expr_operation(
				&operation_expression.operator,
				&operation_expression.left,
				&operation_expression.right,
				output_reg,
				definitions,
			),
			Expression::Number(
				number,
			) => self.cmp_expr_int(&number.value, output_reg, definitions),
			Expression::Tuple(
				tuple,
			) => self.cmp_expr_tuple(
					&tuple.elements,
					output_reg,
					definitions,
				),
			Expression::Var(
				var_expression,
			) => self.cmp_expr_var(
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


	/// Compiles a dot index. (Ex: foo.bar).
	fn cmp_expr_dot_index(
		&self,
		dot_index: &DotIndex,
		output_reg: QuStackId,
		definitions: &mut Definitions,
	)-> Result<QuAsmBuilder, QuMsg> {
		let left_type = self.get_expr_type(
			&dot_index.left,
			definitions,
		)?;
		let left_class = definitions.get_class(left_type)?;
		let indexed_item = left_class.get_item_id(
			&dot_index.right.slice
		)?;

		let builder = match indexed_item {
			ItemId::Class(_) => todo!(),
			ItemId::Constant(_) => todo!(),
			ItemId::ExternalFunction(_) => todo!(),
			ItemId::Function(_) => todo!(),
			ItemId::FunctionGroup(_) => todo!(),
			ItemId::Module(_) => todo!(),
			ItemId::StaticVariable(_) => todo!(),
			ItemId::Variable(variable_id) => {
				let variable = self.context
					.get_variable(variable_id)?;
				self.cmp_copy_register(
					variable.stack_id,
					output_reg,
					definitions
				)?
			},
		};

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

		let item = self.context.find_item(
			&var_expression.name.slice,
			definitions,
		)?;

		let builder = match item {
			ItemId::Class(id) => {
				let mut builder = QuAsmBuilder::new();
				builder.return_type = definitions.class_id::<Class>()?;
				builder
			},
			ItemId::Constant(_) => todo!(),
			ItemId::ExternalFunction(_) => todo!(),
			ItemId::Function(_) => todo!(),
			ItemId::FunctionGroup(_) => todo!(),
			ItemId::Module(_) => {
				let mut builder = QuAsmBuilder::new();
				builder.return_type = definitions.class_id::<Module>()?;
				builder
			},
			ItemId::StaticVariable(_) => todo!(),
			ItemId::Variable(_) => {
				let ItemId::Variable(var_id) = self.context.find_item(
					&var_expression.name.slice,
					&definitions,
				)? else { return Err(format!(
					"Err in cmp_expr_var. TODO"
				).into()) };
				let variable = self.context
					.get_variable(var_id)?;
				
				if variable.stack_id == output_reg {
					// Output and variable locations are the same, do nothing
					let mut b = QuAsmBuilder::default();
					b.return_type = variable.stack_id.class_id();
					return Ok(b);
				}
		
				self.cmp_copy_register(
					variable.stack_id,
					output_reg,
					definitions
				)?
			},
		};

		Ok(builder)
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
					definitions.class_id::<Bool>()?,
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
					definitions.class_id::<Bool>()?,
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

		let name = &call_expression.name.slice;
		let mut function_identity = FunctionIdentity {
			name: name.to_owned(),
			return_type: store_to.class_id(),
			..Default::default()
		};

		let (caller_is_container, caller_id) = {
			if let Some(caller) =  &call_expression.caller {
				let object_id = self.get_expr_type(
					&caller,
					definitions,
				)?;

				(
					object_id == definitions.class_id::<Class>()?
						|| object_id == definitions.class_id::<Module>()?,
					Some(object_id)
				)
			} else {
				(false, None)
			}
		};

		// Compile parameter expressions
		let mut parameter_ids = vec![];
		self.context.open_scope(); {
			if let Some(object_id) = caller_id {
				// Maker caller the first parameter
				if !caller_is_container {
					let var_stack_id = self.context.allocate(
						object_id,
						definitions,
					)?;
					let parameter_expr = self.cmp_expr(
						call_expression.caller.as_ref().unwrap(),
						var_stack_id,
						definitions,
					)?;
					builder.add_builder(parameter_expr);
					parameter_ids.push(var_stack_id);
				}
			}

			for parameter in &call_expression.parameters.elements {
				let object_id = self.get_expr_type(
					parameter,
					definitions,
				)?;
				let var_stack_id = self.context.allocate(
					object_id,
					definitions,
				)?;
				let parameter_expr = self.cmp_expr(
					parameter,
					var_stack_id,
					definitions,
				)?;
				builder.add_builder(parameter_expr);
				parameter_ids.push(var_stack_id);
			}
		}
		self.context.close_scope();
		function_identity.parameters = parameter_ids
			.iter()
			.map(|x| -> ClassId {
				x.class_id()
			})
			.collect();

		// Find function id in the current context
		let id = if caller_is_container {
			let identity = call_expression.caller
				.as_ref()
				.unwrap()
				.into_identity();
			let item = self.context.find_item(
				identity,
				definitions,
			)?;

			match item {
				ItemId::Class(id) => {
					let class = definitions.get_class(id)?;
					let fn_group = class.get_function_group_id(
						&function_identity.name
					)?;
					*definitions.function_groups[fn_group]
						.map
						.get(&function_identity)
						.unwrap() // TODO: Handle none
				},
				ItemId::Module(id) => {
					let module = definitions.get_module(id)?;
					let fn_group = module.get_function_group_id(
						&function_identity.name
					)?;
					*definitions.function_groups[fn_group]
						.map
						.get(&function_identity)
						.unwrap() // TODO: Handle none
				},
				_ => panic!(),
			}
		} else {
			self.context
				.get_some_function_id_by_identity(
					&function_identity,
					&definitions,
				)?
		};

		// Compile the function call
		match id {
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
						let item_id = self.context.find_item(&identity.slice, definitions)?;
						let ItemId::Class(id) = item_id else {panic!()};
						parameters_types.push(id);
						parameters_names.push((param.name().to_owned(), id))
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
				name: parsed_identity.name.slice.clone(),
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
		let mut body_code = {
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
			Ok::<QuAsmBuilder, QuMsg>(b)
		}?;
		self.context.close_frame();

		// Compile function body
		definitions.get_function_mut(func_id)?.code_block = definitions
			.byte_code_blocks
			.len();
		let bytcode = body_code.compile(
			&self.name_refs,
			definitions
		)?;
		definitions.byte_code_blocks.push(bytcode);

		let code = QuAsmBuilder::new();
		return Ok(code);
	}


	/// Compiles an import statement.
	fn cmp_import(
		&mut self,
		import: &Import,
		definitions: &mut Definitions,
	) -> Result<QuAsmBuilder, QuMsg> {
		let next_index= |
			item: ItemId,
			indexer: &str,
			definitions: &Definitions,
		| -> Result<ItemId, QuMsg> {
			Ok(match item {
				ItemId::Class(id) => {
					definitions
						.get_class(id)?
						.get_item_id(indexer)?
				},
				ItemId::Constant(_) => todo!(),
				ItemId::ExternalFunction(_) => todo!(),
				ItemId::Function(_) => todo!(),
				ItemId::FunctionGroup(_) => todo!(),
				ItemId::Module(id) => {
					definitions
						.get_module(id)?
						.get_item_id(indexer)?
				},
				ItemId::StaticVariable(_) => todo!(),
				ItemId::Variable(_) => todo!(),
			})
		};

		let mut identity = &import.identity_path;
		let mut item:Option<ItemId> = None;
		loop {
			match identity {
				Identity::Single(token) => {
					if item.is_none() {
						// First iteration
						let module_id = definitions.get_module_id(
							&token.slice,
						).unwrap();
						item = Some(ItemId::Module(module_id));
						break;
					}

					item = Some(next_index(
						item.unwrap(),
						&token.slice,
						&definitions
					)?);

					break;
				},
				Identity::Index(index) => {
					if item.is_none() {
						// First iteration
						let module_id = definitions.get_module_id(
							&index.left.slice,
						).unwrap();
						item = Some(ItemId::Module(module_id));
						identity = &index.right;
						continue;
					}

					item = Some(next_index(
						item.unwrap(),
						&index.left.slice,
						&definitions
					)?);
					identity = &index.right;
				},
			}
		}

		match item.unwrap() {
			ItemId::Class(id) => {
				self.context.import_class(
					id,
					None,
					definitions
				)?;
			},
			ItemId::Constant(_) => todo!(),
			ItemId::ExternalFunction(_) => todo!(),
			ItemId::Function(_) => todo!(),
			ItemId::FunctionGroup(id) => {
				self.context.import_function(
					id,
					None,
					definitions
				)?;
			},
			ItemId::Module(id) => {
				self.context.import_module(
					id,
					None,
					definitions
				)?;
			},
			ItemId::StaticVariable(_) => todo!(),
			ItemId::Variable(_) => todo!(),
		}

		Ok(QuAsmBuilder::new())
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
						
						code.add_builder(self.cmp_expr(
							expression,
							expression_reg,
							definitions
						)?);
						code.add_builder(self.cmp_copy_register(
							expression_reg,
							return_reg,
							definitions,
						)?);

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
			Statement::Import(import) => {
				return  self.cmp_import(
					import,
					definitions,
				);
			},
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


	fn cmp_module(
		&mut self,
		code_block: &CodeBlock,
		definitions: &mut Definitions,
	) -> Result<QuAsmBuilder, QuMsg> {
		let code = self.cmp_code_block(
			code_block,
			definitions
		)?.compile(&self.name_refs, definitions)?;
		definitions.byte_code_blocks.push(code);
		
		Ok(QuAsmBuilder::new())
	}


	/// Compiles code variable assignment.
	fn cmp_var_assign(
		&mut self,
		var_assignment: &VarAssignment,
		definitions: &mut Definitions,
	) -> Result<QuAsmBuilder, QuMsg> {
		let item = self.context.find_item(
			&var_assignment.name.slice,
			definitions,
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
		if self.context.has_item(
			&var_declaration.name.slice,
			definitions,
		) {
			return Err(format!(
				"An item by '{}' is already defined.", ident,
			).into());
		}

		let static_type = self.class_id_from_option_identity(
			&var_declaration.static_type,
			definitions
		)?;
		let var_stack_id = self.context.define_variable(
			ident.slice.clone(),
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
		
		let base_id = *definitions.module_map
			.get(FUNDAMENTALS_MODULE)
			.unwrap();
		let main_id = *definitions.module_map
			.get(MAIN_MODULE)
			.unwrap();

		self.context.open_frame(ContextFrame::module(main_id));
		{
			// Import base module
			let base_module = definitions.get_module(base_id)?;
			for 
				fn_group_id
				in base_module.function_groups_map.values()
			{
				self.context.import_function(
					*fn_group_id,
					None,
					definitions,
				)?;
			}
			for 
				class_id
				in base_module.class_map.values()
			{
				self.context.import_class(
					*class_id,
					None,
					definitions,
				)?;
			}
		}
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
			let mut code = self.cmp_module(code_block, definitions)?;
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
		return match expr_leaf {
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
			Expression::Tuple(_) => {
				todo!()
			},
			Expression::Var(var) => {
				let item = self.context.find_item(
					&var.name.slice,
					definitions,
				)?;
				let ItemId::Variable(id) = item else { return Err(format!(
					"'{}' is not a variable", var.name,
				).into()) };
				let var = self.context.get_variable(id)?;
				Ok(var.stack_id)
			}
			Expression::DotIndex(_) => {
				todo!()
			},
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
				let item = self.context.find_item(
					&var.name.slice,
					definitions,
				)?;

				let id = match item {
					ItemId::Class(id) => {
						definitions.class_id::<Class>()?
					},
					ItemId::Constant(_) => todo!(),
					ItemId::ExternalFunction(_) => todo!(),
					ItemId::Function(_) => todo!(),
					ItemId::FunctionGroup(_) => todo!(),
					ItemId::Module(id) => {
						definitions.class_id::<Module>()?
					},
					ItemId::StaticVariable(_) => todo!(),
					ItemId::Variable(id) => {
						let var = self.context
							.get_variable(id)?;
						var.stack_id.class_id()
					},
				};
				
				id
			}
			Expression::DotIndex(dot_index) => {
				self.cmp_expr_dot_index(
					dot_index,
					QuStackId::default(),
					definitions,
				)?.return_type
			},
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
			definitions.get_module_id("__main__").unwrap()
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
						name: signature.name.slice.clone(),
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
