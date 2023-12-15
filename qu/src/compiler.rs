
use duplicate::duplicate_item;
use once_cell::sync::Lazy;

use crate::Bool;
use crate::Class;
use crate::Float;
use crate::Int;
use crate::Module;
use crate::vm::QuOp;
use crate::vm::QuOp::*;
use crate::QuParser;
use crate::Register;
use crate::QuStackId;
use crate::Void;
use crate::Uuid;
use crate::import::ExternalFunctionPointer;
use crate::import::ModuleBody;
use crate::import::ModuleBuilder;
use crate::import::QuStruct;
use crate::import::ClassId;
use crate::import::Registerer;
use crate::parser::KEYWORD_BOOL_FALSE;
use crate::parser::KEYWORD_BOOL_TRUE;
use crate::parser::KEYWORD_IF;
use crate::parser::KEYWORD_WHILE;
use crate::parser::QuOperator;
use crate::parser::parsed::*;

use crate::QuMsg;
use crate::QuToken;
use crate::objects::FUNDAMENTALS_MODULE;

use core::fmt;
use core::panic;
use std::any::TypeId;
use std::collections::HashMap;
use std::fmt::Display;
use std::mem::size_of;
use std::hash::Hash;
use std::sync::RwLock;

pub const CONSTRUCTOR_NAME:&str = ".new";
// TODO: Fix compiler's documentation

// TODO: Make bank store which definitions obj the mappings are for
/// Stores a conversion map of types to [`ClassId`]s.
pub(crate) static
	REGISTERED_BANK:Lazy<RwLock< HashMap<Uuid, HashMap<TypeId, ClassId>> >>
	= Lazy::new(||{RwLock::new(HashMap::new())});

pub type ConstantId = usize;

#[derive(Debug, Clone)]
/// Reresents a Qu item.
pub struct CommonItem {
	/// The name of this item.
	pub name: String,

	pub constants_map: HashMap<String, ConstantId>,
	pub class_map: HashMap<String, ClassId>,
	pub function_groups_map: HashMap<String, FunctionGroupId>,
	pub static_variables_map: HashMap<String, VariableId>,
	
	pub(crate) implementations: HashMap<ClassId, Implementation>,
} impl CommonItem {
	pub fn has_item(&self, identity: &str) -> bool {
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


	#[duplicate_item(
		get_trait_implementation Self Implementation _get;
		[get_trait_implementation] [Self] [Implementation] [get];
		[get_trait_implementation_mut] [mut Self] [mut Implementation] [get_mut];
	)]
	pub(crate) fn get_trait_implementation(
		self: &Self,
		trait_id: ClassId,
	) -> Result<&Implementation, QuMsg> {
		self.implementations
			._get(&trait_id)
			.ok_or_else(||->QuMsg {
				format!(
					"Item '{}' has no implementation for trait '{:?}'",
					self.name,
					trait_id
				).into()
			})
	}


	pub fn get_item_id(&self, identity: &str) -> Result<ItemId, QuMsg> {
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
			"Item '{}' has no item by name '{}'.", self.name, identity,
		).into())
	}

} impl Default for CommonItem {
	fn default() -> Self {
		Self {
			name: "".into(),
			class_map: Default::default(),
			constants_map : Default::default(),
			function_groups_map: Default::default(),
			static_variables_map: Default::default(),
			implementations: Default::default(),
		}
	}
}


#[derive(Debug)]
pub struct Constant {
	pub name: String,
	pub value: Box<[u8]>,
	pub class_id: ClassId,
}

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
		frame.scopes.pop().unwrap();
	}


	fn find_function(
		&self,
		identity: &FunctionIdentity,
		definitions: &Definitions,
	) -> Result<FunctionId, QuMsg> {
		self.find_function_maybe(identity, definitions)
		.ok_or_else(|| -> QuMsg { format!(
			"Could not find a function with identity '{identity}' in \
			the current context."
		).into()})
	}


	fn find_function_maybe(
		&self,
		identity: &FunctionIdentity,
		definitions: &Definitions,
	) -> Option<FunctionId> {
		let item = self.find_in_stack_filtered(
			&identity.name,
			&|item| {
				if let ItemId::Function(_) = item {
					return true;
				}
				if let ItemId::FunctionGroup(_) = item {
					return true;
				}
				false
			},
		);
		if let Some(function_id) = item {
			return match function_id {
				ItemId::Function(function_id) => {
					Some( function_id )
				},
				ItemId::FunctionGroup(id) => {
					definitions.function_groups[id]
						.map
						.get(identity)
						.map(|x|{*x})
				},
				_ => unreachable!(),
			};
		}

		let item = 'frames: {
			for frame in self.frames.iter().rev() {
				match frame {
					ContextFrame::Module(module_id, _) => {
						let module = definitions.get_module(
							*module_id
						).unwrap();
						let mut item_opt = None;
						if let Some(group_id) =
							module.common.function_groups_map.get(&identity.name)
						{
							if let Some(some_fn_id) =
								definitions.function_groups[*group_id]
									.map
									.get(identity)
							{
								item_opt = Some(*some_fn_id)
							}
						}

						if let Some(item) = item_opt {
							break 'frames Some(item);
						}
					},
					ContextFrame::Class(class_id, _) => {
						let class = definitions.get_class(
							*class_id
						).unwrap();
						let mut item_opt = None;
						if let Some(group_id) =
							class.common.function_groups_map.get(&identity.name)
						{
							if let Some(some_fn_id) =
								definitions.function_groups[*group_id]
									.map
									.get(identity)
							{
								item_opt = Some(*some_fn_id)
							}
						}

						if let Some(item) = item_opt {
							break 'frames Some(item);
						}
					},
					ContextFrame::Function(_function_id, _) => {
						continue;
					},
				}
			}

			None
		};

		let Some(item) = item else {
			return None;
		};

		Some(item)
	}


	fn find_in_stack_filtered(
		&self,
		identity:&str,
		filter: &dyn Fn(ItemId) -> bool,
	) -> Option<ItemId> {
		for frame in self.frames.iter().rev() {
			let frame_data = frame.get_frame();
			for scope in frame_data.scopes.iter().rev() {
				let item_opt = scope
					.find_item(identity);
				if let Some(item) = item_opt {
					if filter(item) {
						return Some(item);
					}
				}
			}
		}

		None
	}


	fn find_item(
		&self,
		identity: &str,
		definitions: &Definitions,
	) -> Result<ItemId, QuMsg> {
		self.find_item_filtered(identity, &|_| {true}, definitions)
	}


	fn find_item_maybe(
		&self,
		identity: &str,
		definitions: &Definitions,
	) -> Option<ItemId> {
		self.find_item_filtered_maybe(identity, &|_| {true}, definitions)
	}


	fn find_item_filtered(
		&self,
		identity: &str,
		filter: &dyn Fn(ItemId) -> bool,
		definitions: &Definitions,
	) -> Result<ItemId, QuMsg> {
		let item = self.find_item_filtered_maybe(
			identity,
			filter,
			definitions
		);

		let Some(item) = item else {
			return Err(format!(
				"Could not find anything with name '{}' in current context",
				identity,
			).into());
		};

		Ok(item)
	}
	
	fn find_item_filtered_maybe(
		&self,
		identity: &str,
		filter: &dyn Fn(ItemId) -> bool,
		definitions: &Definitions,
	) -> Option<ItemId> {
		let item = self.find_in_stack_filtered(identity, filter);
		if let Some(item) = item {
			return Some(item);
		}
		

		let item = 'frames: {
			for frame in self.frames.iter().rev() {
				match frame {
					ContextFrame::Module(module_id, _) => {
						let module = definitions.get_module(
							*module_id
						).unwrap();
						let item_opt = if
							!module.common.has_item(identity)
						{
							None
						} else {
							Some(module.common.get_item_id(identity).unwrap())
						};

						if let Some(item) = item_opt {
							if filter(item) {
								break 'frames Some(item);
							}
						}
					},
					ContextFrame::Class(class_id, _) => {
						let class = definitions
							.get_class(*class_id).unwrap();
						let item_opt = if
							!class.has_item(identity)
						{
							None
						} else {
							Some(class.get_item_id(identity).unwrap())
						};

						if let Some(item) = item_opt {
							if filter(item) {
								break 'frames Some(item);
							}
						}
					},
					ContextFrame::Function(_function_id, _) => {
						continue;
					},
				}
			}

			None
		};

		let Some(item) = item else {
			return None;
		};

		Some(item)
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


	pub fn get_function_id_by_identity(
		&self,
		identity: &FunctionIdentity,
		definitions: &Definitions,
	) -> Result<FunctionId, QuMsg> {
		let item = self.find_item_filtered_maybe(
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
		);
		if let Some(item) = item {
			let ItemId::FunctionGroup(function_group) = item else {
				return Err(format!(
					"error in Context::get_some_function_id_by_identity TODO",
				).into());
			};
			if definitions
				.get_function_group(function_group)?
				.map
				.contains_key(identity)
			{
				// Found function in function group
				return Ok(
					*definitions
						.get_function_group(function_group)?
						.map
						.get(identity).unwrap()
				);
			}
		}
		

		if identity.parameters.is_empty() {
			return Err(format!(
				"Function needs at least one argument. TODO: Better msg",
			).into());
		}
		let source_class_id = identity.parameters[0];
		let class = definitions.get_class(source_class_id)?;


		// Find function in trait implementation
		if !class.has_item(&identity.name) {
			for (trait_id, _imp) in &class.common.implementations {
				let trait_data = definitions.get_class(*trait_id)?;
				if !trait_data.has_item(&identity.name) {
					continue;
				}
				let ItemId::FunctionGroup(trait_group_id) = trait_data.common
					.get_item_id(&identity.name)?
					else {
						continue;
					};
				let trait_fn_id = definitions
					.get_function_group(trait_group_id)?
					.get_fn_id(&identity, definitions)?;
				let ItemId::Function(implemented_fn_id) = class.common
					.get_trait_implementation(*trait_id)?
					.get_implemented_item(ItemId::Function(trait_fn_id))?
					else {
						unreachable!();
					};
				return Ok(implemented_fn_id);
			}
		}

		// Find item in class
		let group_id = definitions
			.get_class(source_class_id)?
			.get_function_group_id(&identity.name)?;
		let fn_id = definitions
			.get_function_group(group_id)?
			.get_fn_id(&identity, definitions)?;

		Ok(fn_id)
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
						break 'frames module.common.has_item(identity);
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


	fn import_class_or_module(
		&mut self,
		id: ItemId,
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
		scope.import_class_or_module(id, alias, definitions)?;

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


pub type RegistrationMethod = dyn Fn(&mut Registerer) -> Result<(), QuMsg>;
#[derive(Debug, Default)]
pub struct Definitions {
	pub(crate) uuid: Uuid,
	pub constants: Vec<Constant>,
	pub classes: Vec<QuStruct>,
	pub functions: Vec<FunctionMetadata>,
	pub function_groups: Vec<FunctionGroup>,
	pub modules: Vec<ModuleMetadata>,

	pub(crate) byte_code_blocks: Vec<Vec<QuOp>>,

	/// A map of names to module IDs.
	pub module_map: HashMap<String, ModuleId>,
	/// All registered classes with external functions that were not registered.
	with_unregistered_functions: Vec<ClassId>, // TODO: Remove with_unregistered_functions
} impl Definitions {
	pub fn new(uuid: Uuid) -> Self {
		let mut d = Self::default();
		d.uuid = uuid;
		REGISTERED_BANK.write().unwrap().insert(uuid, HashMap::default());
		d
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
		let group_id = match &class.common.function_groups_map.get(name) {
			Some(id) => {**id},
			None => {
				let module = self.get_module_mut(module_id)?;
				let id = match &module.common.function_groups_map.get(name) {
			Some(id) => {**id},
			None => {
				let id = new_id;
						module.common.function_groups_map.insert(
							name.into(), id,
						);
						let mut group = FunctionGroup::default();
						group.name = name.clone();
						self.function_groups.push(group);
						id
					}
				};
				let class = self.get_class_mut(class_id)?;
				class.common.function_groups_map.insert(
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
			let module_name = &self.get_module(module_id).unwrap().common.name;
			return Err(format!(
				"Couldn't register function '{signature}' in module \
				'{module_name}'. A function with that signature \
				already exists."
			).into())
		}
		group.map.insert(signature.clone(), id); // TODO: Remove cloning

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
		let group_id = match &class.common.function_groups_map.get(name) {
			Some(id) => {**id},
			None => {
				let id = new_id;
				class.common.function_groups_map.insert(
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
		group.map.insert(signature.clone(), id); // TODO: Remove cloning

		Ok(())
	}


	/// For registering functions that are not assocaiated with a class.
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
		let group_id = match &module.common.function_groups_map.get(name) {
			Some(id) => {**id},
			None => {
				let id = new_id;
				module.common.function_groups_map.insert(
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
		group.map.insert(signature.clone(), id); // TODO: Remove cloning
		Ok(())
	}


	/// Adds function group mappings to the function with the given signature.
	/// 
	/// Also does the same for class of the first argument in the signature.
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
		let group_id = match &module.common.function_groups_map.get(name) {
			Some(id) => {**id},
			None => {
				let id = new_id;
				module.common.function_groups_map.insert(
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
		group.map.insert(signature.clone(), id); // TODO: Remove cloning
		// Add to class's mappings
		let class_id = signature.id_self_class();
		self.add_class_function_map(class_id, signature, id)?;

		Ok(())
	}


	pub fn add_to_function_group(
		&mut self,
		item_id: ItemId,
		fn_id: FunctionId,
	) -> Result<(), QuMsg> {
		let identity = self.functions[fn_id].identity.clone();

		let new_group_id = self.function_groups.len();
		let function_group_map = match item_id {
			ItemId::Class(id) => {
				&mut self.get_class_mut(id)?.common.function_groups_map
			},
			ItemId::Constant(_) => todo!(),
			ItemId::Function(_) => todo!(),
			ItemId::FunctionGroup(_) => todo!(),
			ItemId::Module(id) => {
				&mut self.get_module_mut(id)?.common.function_groups_map
			},
			ItemId::StaticVariable(_) => todo!(),
			ItemId::Variable(_) => todo!(),
			ItemId::None => todo!(),
		};

		let group_id = if
			!function_group_map.contains_key(&identity.name)
		{
			function_group_map.insert(identity.name.clone(), new_group_id);
			self.function_groups.push(
				FunctionGroup::new(identity.name.clone())
			);
			new_group_id
		} else {
			*function_group_map.get(&identity.name).unwrap()
		};

		let group = &mut self.function_groups[group_id];
		if group.map.contains_key(&identity) {
			todo!("Return error here")
		}
		group.map.insert(identity.clone(), fn_id);

		Ok(())
	}


	pub fn class_id<T:Register>(
		&self
	) -> Result<ClassId, QuMsg> {
		self.find_class_id(<T as Register>::name())
	}

	/// Adds a constant without binding any names to it.
	pub fn add_constant<T: Register + 'static>(
		&mut self,
		name: String,
		value: T,
	) -> Result<ConstantId, QuMsg> {
		let Some(class_id) = T::get_id(&self.uuid)
			else {
				return Err("TODO: Add proper err msg".into())
			};

		let allocated_value_ptr:*mut T = Box::leak(Box::new(value));
		let vec = unsafe { Vec::from_raw_parts(
			allocated_value_ptr as *mut u8,
			size_of::<T>(),
			size_of::<T>(),
		) };
		let constant = Constant {
			name: name,
			value: vec.into_boxed_slice(),
			class_id,
		};
		let constant_id = self.constants.len();
		self.constants.push(constant);
		Ok(constant_id)
	}


	/// Adds a constant and binds a names to it relative to the given item.
	pub fn define_constant_in_item<T: Register + 'static>(
		&mut self,
		name: String,
		value: T,
		item_id: ItemId,
	) -> Result<ConstantId, QuMsg> {
		let constant_id = self.add_constant(name.clone(), value)?;

		let constants_map = match item_id {
			ItemId::Class(_) => todo!(),
			ItemId::Constant(_) => todo!(),
			ItemId::Function(_) => todo!(),
			ItemId::FunctionGroup(_) => todo!(),
			ItemId::Module(id) => 
				&mut self.get_module_mut(id)?.common.constants_map,
			ItemId::StaticVariable(_) => todo!(),
			ItemId::Variable(_) => todo!(),
			ItemId::None => todo!(),
		};
		constants_map.insert(name, constant_id);

		Ok(constant_id)
	}


	pub fn define_function(
		&mut self,
		identity: FunctionIdentity,
	) -> Result<FunctionId, QuMsg> {
		let funciton_id = self.functions.len();

		// Update module's function map
		let first_arg_id = match identity.parameters.first() {
			Some(id) => *id,
			None => self.class_id::<Void>()?,
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

	pub fn define_function_in_item(
		&mut self,
		identity: FunctionIdentity,
		item_id: ItemId,
	) -> Result<FunctionId, QuMsg> {
		let funciton_id = self.functions.len();

		// Update module's function map
		let first_arg_id = match identity.parameters.first() {
			Some(id) => *id,
			None => self.class_id::<Void>()?,
		};

		// Push to functions
		let function_id = self.functions.len();
		self.functions.push(FunctionMetadata {
			identity: identity.clone(),
			..Default::default()
		});

		match item_id {
			ItemId::Class(_) => todo!(),
			ItemId::Constant(_) => todo!(),
			ItemId::Function(_) => todo!(),
			ItemId::FunctionGroup(_) => todo!(),
			ItemId::Module(id) => {
				self.add_module_function_map(
					id,
					identity,
					function_id
				)?;
			},
			ItemId::StaticVariable(_) => todo!(),
			ItemId::Variable(_) => todo!(),
			ItemId::None => {
				// Adding a class mapping is not handling automaticly,
				// do it here
				self.add_class_function_map(
					first_arg_id,
					identity.clone(),
					funciton_id,
				)?;
			},
		}

		

		Ok(funciton_id)
	}


	pub fn define_module(
		&mut self,
		name: String,
		body: &ModuleBody,
	) -> Result<ModuleId, QuMsg> {
		if self.module_map.contains_key(&name) {
			return Err(format!(
				"Couldn't register module '{name}'. A module with that name has already been registered."
			).into())
		}
		let module_id = self.modules.len();
		let mut common = CommonItem::default();
		common.name = name.clone();
		self.modules.push(ModuleMetadata {common});
		self.module_map.insert(name, module_id);
		
		assert_eq!(self.with_unregistered_functions.len(), 0, "Some functions were not registered");
		let mut builder = ModuleBuilder {
			definitions: self,
			module_id,
		};
		(body)(&mut builder)?;
		assert_eq!(self.with_unregistered_functions.len(), 0, "Some functions were not registered");
		
		Ok(module_id)
	}


	pub fn define_module_class(
		&mut self,
		module_id: ModuleId,
		name: String,
	) -> Result<ClassId, QuMsg> {
		let id = ClassId::from(self.classes.len());
		self.classes.push(QuStruct {
			size: 0,
			common: CommonItem {
				name: name.clone(),
				..Default::default()
			},
			..Default::default()
		});
		let module = self.get_module_mut(module_id)?;
		assert!(!module.common.class_map.contains_key(&name));
		module.common.class_map.insert(name, id);
		
		Ok(id)
	}


	pub fn define_module_function(
		&mut self,
		_module_id: ModuleId,
		identity: FunctionIdentity,
	) -> Result<FunctionId, QuMsg> {

		// TODO: Determin if static function mappings should be kept
		// if let Some(class_id) = class_id {
		// 	self.add_static_function_mapping_in_class(
		// 		*class_id,
		// 		identity.clone(), // TODO: Remove cloning
		// 	)?;
		// }
		// self.add_static_function_mapping_in_module(
		// 	module_id,
		// 	identity.clone(), // TODO: Remove cloning
		// )?;

		let id = FunctionId::from(self.functions.len());
		self.functions.push(FunctionMetadata {
			identity: identity,
			..Default::default()
		});
		
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


	pub(crate) fn get_class_mut(
		&mut self, id: ClassId,
	) -> Result<&mut QuStruct, QuMsg> {
		let class = self.classes
			.get_mut(usize::from(id))
			.ok_or_else(|| -> QuMsg { format!(
				"There's no class with id '{:?}' defined.", id,
			).into()})?;
		Ok(class)
	}


	fn get_common_of(&self, item:ItemId) -> Result<&CommonItem, QuMsg>{
		match item {
			ItemId::Class(id) => Ok(&self.get_class(id)?.common),
			ItemId::Constant(_) => todo!(),
			ItemId::Function(_) => todo!(),
			ItemId::FunctionGroup(_) => todo!(),
			ItemId::Module(id) => Ok(&self.get_module(id)?.common),
			ItemId::StaticVariable(_) => todo!(),
			ItemId::Variable(_) => todo!(),
			ItemId::None => todo!(),
		}
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


	pub(crate) fn get_item_in_impl(
		&self,
		name: &str,
		item_id: ItemId,
		trait_id: ClassId,
	) -> Result<ItemId, QuMsg> {
		let trait_item = self.get_common_of(
			ItemId::Class(trait_id)
		)?.get_item_id(name)?;

		let class_id = self.get_common_of(item_id)?;
		class_id.implementations
			.get(&trait_id)
			.unwrap()
			.get_implemented_item(trait_item)
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

	
	fn find_class_id(&self, name: &str) -> Result<ClassId, QuMsg> {
		for module in &self.modules {
			if module.common.class_map.contains_key(name) {
				return Ok(*module.common.class_map.get(name).unwrap());
			}
		}
		Err(format!(
			"There's no class with name '{}' defined.", name,
		).into())
	}


	pub(crate) fn impl_trait_in_item(
		&mut self,
		trait_id: ClassId,
		class_id: ClassId,
	) -> Result<(), QuMsg>{
		let trait_common = self.get_class(trait_id)?.common.clone();
		
		let mut implementation = Implementation::default();

		for (_, group_id) in trait_common.function_groups_map {
			let group = self.get_function_group(group_id)?;
			for (_, fn_id) in &group.map {
				implementation.functions.insert(*fn_id, *fn_id);
			}
		}

		let class_data = self.get_class_mut(class_id)?;
		if class_data.common.implementations.contains_key(&trait_id) {
			return Err(format!(
				"TODO: Add err msg"
			).into());
		}
		class_data.common.implementations.insert(
			trait_id,
			implementation,
		);
		Ok(())
	}


	pub fn register(
		&mut self,
		body:&RegistrationMethod,
	) -> Result<(), QuMsg> {
		let mut registerer = Registerer {
			definitions: self,
		};
		body(&mut registerer)?;
		Ok(())
	}


	/// Registers an external function can only only be through either
	/// the module or the class..
	/// 
	/// # Examples
	/// 
	/// ``` qu
	/// int.add(2, 3) # Valid
	/// 2.add(3) # Valid
	/// add(2, 3) # Valid
	/// ```
	pub fn register_function_in_module(
		&mut self,
		module_id: ModuleId,
		external_function:FunctionMetadata,
	) -> Result<(), QuMsg> {
		// Get class id of first parameter
		let class_id = match external_function.identity.parameters.first() {
			Some(class_id) => *class_id,
			None => {
				// Register static function with module, but not a class.

				// Add identity to class's and module's map
				// self.add_static_external_function_mapping_in_module(
				// 	module_id,
				// 	external_function.clone(),
				// )?;
				// Add external function to list
				self.functions.push(external_function);

				return Ok(());
			},
		};

		// Also register function with module.
		self.register_static_function_in_class(
			class_id,
			external_function.clone(),
		)?;
		self.register_static_function_in_module(
			module_id,
			external_function.clone(),
		)?;

		Ok(())
	}


	pub fn register_function_implementation(
		&mut self,
		class_id:ClassId,
		trait_id:ClassId,
		external_function:FunctionMetadata,
	) -> Result<(), QuMsg> {
		let new_impl_fn_id = self.functions.len();

		// Map implemenetation
		let trait_common = self.get_class(trait_id)?.common.clone();
		let trait_group_id = trait_common
			.get_function_group_id(&external_function.identity.name)?;
		let old_trait_fn_id = self.get_function_group(trait_group_id)?
			.get_fn_id(&external_function.identity, self)?;

		// Override old implementation mapping
		self.get_class_mut(class_id)?
			.common
			.get_trait_implementation_mut(trait_id)?
			.functions
			.insert(old_trait_fn_id, new_impl_fn_id);

		self.functions.push(external_function);


		Ok(())
	}


	/// Registers an external function can only only be accessed through
	/// the class.
	/// 
	/// # Examples
	/// 
	/// ``` qu
	/// int.add(2, 3) # Valid
	/// 2.add(3) # Valid
	/// 
	/// add(2, 3) # Invalid
	/// ```
	pub fn register_static_function_in_class(
		&mut self,
		class_id: ClassId,
		external_function:FunctionMetadata,
	) -> Result<(), QuMsg> {
		// Add function to global list
		let new_function_id = self.functions.len();
		self.functions.push(external_function);

		self.add_to_function_group(
			ItemId::Class(class_id),
			new_function_id
		)?;

		Ok(())
	}


	/// Registers an external function can only only be accessed through
	/// the module.
	/// 
	/// # Examples
	/// 
	/// ``` qu
	/// add(2, 3) # Valid
	/// 
	/// int.add(2, 3) # Invalid
	/// 2.add(3) # Invalid
	/// ```
	pub fn register_static_function_in_module(
		&mut self,
		module_id: ModuleId,
		external_function:FunctionMetadata,
	) -> Result<(), QuMsg> {
		// Also register function with module.
		// self.add_static_external_function_mapping_in_module(
		// 	module_id,
		// 	external_function.clone(),
		// )?;
		// Add function to global list
		let new_function_id = self.functions.len();
		self.functions.push(external_function);

		self.add_to_function_group(
			ItemId::Module(module_id),
			new_function_id
		)?;

		Ok(())
	}


	/// Registers an external struct to be used within the Qu langauge.
	pub fn register_module_struct<S:Register+'static>(
		&mut self,
		module_id: ModuleId,
	) -> Result<ClassId, QuMsg> {
		let class_name = <S as Register>::name();

		// Manage classes map
		let class_id:ClassId = self.classes.len().into();
		let module = self.get_module_mut(module_id)?;
		assert!(!module.common.class_map.contains_key(class_name));
		module.common.class_map.insert(class_name.into(), class_id);

		// Add class to list of classes
		let class = QuStruct::new(
			class_name,
			size_of::<S>(),
		);
		self.classes.push(class);

		// Add struct to register bank
		REGISTERED_BANK
			.write()
			.unwrap()
			.get_mut(&self.uuid)
			.unwrap()
			.insert(TypeId::of::<S>(), class_id);

		Ok(class_id)

	}
}


pub type ExternalFunctionId = usize;


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
	map: HashMap<FunctionIdentity, FunctionId>,
	name: String,
} impl FunctionGroup {
	fn new(name: String) -> Self {
		Self {
			name,
			..Default::default()
		}
	}

	fn get_fn_id(
		&self,
		by_identity:&FunctionIdentity,
		definitions:&Definitions,
	) -> Result<FunctionId, QuMsg> {
		for (identity, fn_id) in &self.map {
			if !by_identity.compare(identity, definitions) {
				continue;
			}
			return Ok(*fn_id);
		}
		return Err(format!(
			"No function by identity {} exists.",
			by_identity
		).into());
	}
}

pub type FunctionGroupId = usize;

pub type FunctionId = usize;


#[derive(Clone, Debug, Default, Eq, Ord)]
pub struct FunctionIdentity {
	pub name: String,
	pub parameters: Box<[ClassId]>,
	pub return_type: ClassId,
} impl FunctionIdentity {
	fn compare(&self, other:&Self, definitions:&Definitions) -> bool {
		if self.name != other.name {
			return false;
		}
		if self.parameters.len() != other.parameters.len() {
			return false;
		}

		for (s_id, o_id) in self.parameters.iter().zip(other.parameters.iter()) {
			if
				s_id != o_id
				&& !definitions.get_class(*s_id)
					.unwrap()
					.common
					.implementations
					.contains_key(&o_id)
			{
				return false;
			}
		}

		return true;
	}

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
		for p in self.parameters.iter() {
			let id = p.0;
			params.push_str(&format!("type:{id}"));
		}
		write!(f,"{}({}) type:{}", self.name, params, self.return_type.0)
	}
}


#[derive(Clone)]
pub(crate) enum FunctionReference {
	Internal(usize),
	External(&'static ExternalFunctionPointer),
} impl fmt::Debug for FunctionReference {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Internal(arg0) =>
				f.debug_tuple("Internal").field(arg0).finish(),
            Self::External(_) =>
				f.debug_tuple("External").field(&()).finish(),
        }
    }
} impl Default for FunctionReference {
    fn default() -> Self {
        FunctionReference::Internal(0)
    }
}

#[derive(Debug, Default, Clone)]
pub struct FunctionMetadata {
	pub identity: FunctionIdentity,
	/// The value that the VM's program counter should be set to in order to
	/// start this function.
	pub(crate) code_block: FunctionReference,
}

#[derive(Debug, Default, Clone)]
pub(crate) struct Implementation {
	functions: HashMap<FunctionId, FunctionId>,
} impl Implementation {
	fn get_implemented_item(&self, item_id:ItemId) -> Result<ItemId, QuMsg> {
		if let ItemId::Function(id) = item_id {
			return Ok(ItemId::Function(*self.functions.get(&id).unwrap()));
		}
		Err("".into())
	}
}

pub type ModuleId = usize;

#[derive(Debug, Default, Clone)]
/// Reresents a Qu module.
pub struct ModuleMetadata {
	pub common: CommonItem,
}

#[derive(Debug, Clone, Copy)]
pub enum ItemId {
	Class(ClassId),
	Constant(ConstantId),
	Function(FunctionId),
	FunctionGroup(FunctionGroupId),
	Module(ModuleId),
	StaticVariable(VariableId),
	Variable(VariableId),
	None,
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
		let item_id = self.definitions_map
			.get(name)
			.map_or_else(
				|| {
					if
						let Some(id) = self.function_groups_map
							.get(name)
					{
						Some(ItemId::FunctionGroup(*id))
					} else {
						None
					}
				},
				|item_id| {Some(*item_id)}
			);
		item_id
	}


	fn import_class_or_module(
		&mut self,
		id: ItemId,
		alias: Option<String>,
		definitions: &Definitions
	) -> Result<(), QuMsg>{
		let name = match alias {
			Some(name) => name,
			None => {
				let item_function_groups_map = match id {
					ItemId::Class(id) => {
						definitions.get_class(id)?.common.name.clone()
					},
					ItemId::Constant(_) => todo!(),
					ItemId::Function(_) => todo!(),
					ItemId::FunctionGroup(_) => todo!(),
					ItemId::Module(id) => {
						definitions.get_module(id)?.common.name.clone()
					},
					ItemId::StaticVariable(_) => todo!(),
					ItemId::Variable(_) => todo!(),
					ItemId::None => todo!(),
				};
				item_function_groups_map
			},
		};

		self.definitions_map.insert(
			name,
			id,
		);
		let item_function_groups_map = match id {
			ItemId::Class(id) => {
				&definitions.get_class(id)?.common.function_groups_map
			},
			ItemId::Constant(_) => todo!(),
			ItemId::Function(_) => todo!(),
			ItemId::FunctionGroup(_) => todo!(),
			ItemId::Module(id) => {
				&definitions.get_module(id)?.common.function_groups_map
			},
			ItemId::StaticVariable(_) => todo!(),
			ItemId::Variable(_) => todo!(),
			ItemId::None => todo!(),
		};
		for
			(group_name, group_id)
			in item_function_groups_map.iter()
		{
			if self.function_groups_map.contains_key(group_name) {
				// TODO: Handle when multiple function groups of the same name are imported
			}
			self.function_groups_map.insert(group_name.clone(), *group_id);
		}

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
				module.common.name.clone()
			},
		};

		self.definitions_map.insert(
			name,
			ItemId::Module(module_id),
		);

		Ok(())
    }
}


type VariableId = usize;


#[derive(Debug, Default, Clone)]
struct VariableMetadata {
	stack_id: QuStackId,
}


/// Compiles [QuLeaf]s into Qu bytecode.
#[derive(Debug, Default, Clone)]
pub struct QuCompiler {
	context: Context,
} impl QuCompiler {
	/// Creates and returns a new [QuCompiler].
	pub fn new() -> Self {
		let inst = Self {
			..Default::default()
		};

		return inst;
	}


	fn class_id_from_call_expression(
		&self,
		call_expression: &CallExpression,
		definitions: &mut Definitions,
	) -> Result<ClassId, QuMsg> {
		let caller_id = match &call_expression.caller {
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
					definitions.class_id::<Void>()?
				}
			},
		};

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

		// Construct function identity
		let mut function_identity = FunctionIdentity {
			name: call_expression.name.slice.clone(),
			parameters: parameters.into_boxed_slice(),
			..Default::default()
		};

		if caller_id == definitions.class_id::<Module>()? {
			if
				let Some(Expression::Var(var_expression))
				= &call_expression.caller
			{
				let ItemId::Module(module_id) = self.context.find_item(
					&var_expression.name.slice,
					definitions
				)? else {
					todo!("Proper error msg");
				};
				let module = definitions.get_module(module_id)?;
				let function_group_id = module.common.get_function_group_id(
					&call_expression.name.slice	
				)?;
				let function_group = &definitions.function_groups[function_group_id];
				let fn_id = *function_group.map
					.get(&function_identity)
					.expect("TODO: Handle error");
						return Ok(
					definitions.get_function(fn_id)?.identity.return_type
						);

			} else {
				todo!("Handle constant evalution of modules properly");
			}
		}

		let callable_id = self.context
			.find_item(&call_expression.name.slice, definitions)?;

		// Get function group
		let fn_id = if let ItemId::Class(class_id) = callable_id {
			function_identity.name = CONSTRUCTOR_NAME.into();
			let class = definitions.get_class(class_id)?;
			let group_id = class.common.function_groups_map
				.get(CONSTRUCTOR_NAME)
				.map(|x|{*x})
				.expect("TODO: Add error msg1");
			let fn_id = *definitions
				.get_function_group(group_id)?
				.map
				.get(&function_identity)
				.expect("TODO: Add error msg2");
			fn_id
		} else {
			if
				caller_id == definitions.class_id::<Class>()?
			{
				todo!("Class dot notation has not been implemented. Requires constant evaluation.");
			} else {
				self.context.find_function(&function_identity, &definitions)?
			}
		};

		// Get return type
				let return_type_id = definitions
			.get_function(fn_id)?
					.identity
					.return_type;
		Ok(return_type_id)
	}


	fn class_id_from_option_identity(
		&self,
		option_identity: &Option<QuToken>,
		definitions: &mut Definitions,
	) -> Result<ClassId, QuMsg> {
		let Some(identity) = option_identity else {
			return definitions.class_id::<Void>();
		};
		let class_id = definitions.find_class_id(
			&identity.slice
		)?;
		
		Ok(class_id)
	}


	fn get_class_constructor_id(
		&mut self,
		class_id: ClassId,
		function_identity: FunctionIdentity,
		definitions: &mut Definitions,
	) -> Result<FunctionId, QuMsg> {
		let class = definitions.get_class(class_id)?;
		let Some(group_id) = class.common.function_groups_map
			.get(CONSTRUCTOR_NAME)
			.map(|x|{*x})
			else {
				return Err("TODO: Add err msg".into())
			};
		let init_signature = FunctionIdentity {
			name: CONSTRUCTOR_NAME.into(),
			parameters: function_identity.parameters,
			..Default::default()
		};
		
		let Some(some_fn_id) = definitions
			.function_groups[group_id]
			.map
			.get(&init_signature)
			.map(|x|{*x})
			else {
				return Err("TODO: Add err msg".into())
			};
		
		Ok(some_fn_id)
		
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
			parameters: Box::new([from.class_id()]),
			return_type: to.class_id(),
		};
		let mut b = QuAsmBuilder::new();
		let group_id = *definitions.get_class(from.class_id())?
			.common
			.function_groups_map
			.get(&identity.name)
			.expect("TODO: Handle None case");
		let fn_id = *definitions.function_groups[group_id]
			.map
			.get(&identity)
			.expect("TODO: Handle None case");
		b.add_op(QuOp::Call(
			fn_id,
			Box::new([from]),
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
		let mut x = 5;
		x += 1;
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
			Expression::Bool(
				bool_literal,
			) => self.cmp_expr_bool(&bool_literal.value, output_reg, definitions),
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
			Expression::Number(number) =>
				self.cmp_expr_number(
					&number.value,
					&number.decimal,
					output_reg,
					definitions
				),
			}?;

		// Type check
		let void_id = definitions.class_id::<Void>()?;
		if builder.return_type != void_id {
			if builder.return_type != output_reg.class_id() {
				return Err(format!(
					"Attempted to assign a value of type '{}' to a location of type '{}'.",
					definitions.get_class(builder.return_type)?.common.name,
					definitions.get_class(output_reg.class_id())?.common.name,
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
			ItemId::None => todo!(),
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
		// let trait_id = match operator {
		// 	x if x == OP_EXPR_ADD => {
		// 		definitions.class_id::<QuAdd>()?
		// 	},
		// 	_ => unimplemented!(),
		// };




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
	fn cmp_expr_bool(
		// TODO: Change output_reg to a struct without type information
		&mut self,
		value:&QuToken,
		output_reg:QuStackId,
		definitions: &mut Definitions,
	) -> Result<QuAsmBuilder, QuMsg> {
		let mut b = QuAsmBuilder::new();

		// WARNING: Value takes an isize, which can be 4 or 8 bytes, but bool is only 4 bytes
		// TODO: Add proper way of adding bool
		match value {
			x if x == KEYWORD_BOOL_TRUE => {
				b.add_op(Value(1, output_reg));
				b.return_type = definitions.class_id::<Bool>()?;
			},
			x if x == KEYWORD_BOOL_FALSE => {
				b.add_op(Value(0, output_reg));
				b.return_type = definitions.class_id::<Bool>()?;
			}
			_ => unreachable!()
		}

		Ok(b)
	}


	fn cmp_expr_number(
		// TODO: Change output_reg to a struct without type information
		&mut self,
		value:&QuToken,
		decimal:&Option<QuToken>,
		output_reg:QuStackId,
		definitions: &mut Definitions,
	) -> Result<QuAsmBuilder, QuMsg> {
		let int = Int::get_id(&definitions.uuid).unwrap();
		let float = Float::get_id(&definitions.uuid).unwrap();
		let mut b = QuAsmBuilder::new();
		match output_reg.class_id() {
			x if x == int => {
				let Ok(val) = value.slice.parse::<Int>() else {
					panic!("Could not convert text '{}' to number!", value.slice);
				};
				b.add_op(Value(val as isize, output_reg));
				b.return_type = int;
			},
			x if x == float => {
				let deci = match decimal {
					Some(tk) => &tk.slice,
					None => ""
				};
				let float_full = format!(
					"{}.{}",
					value.slice,
					deci,
				);
				let Ok(val) = float_full.parse::<Float>() else {
					panic!("Could not convert text '{}' to number!", value.slice);
				};
				b.add_op(Value(val.to_bits() as isize, output_reg));
				b.return_type = float;
			},
			_ => {
				unreachable!();
			}
		}

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
			ItemId::Class(_) => {
				let mut builder = QuAsmBuilder::new();
				builder.return_type = definitions.class_id::<Class>()?;
				builder
			},
			ItemId::Constant(id) => {
				let mut builder = QuAsmBuilder::new();
				builder.add_op(QuOp::LoadConstant(id as u32, output_reg));
				builder
			},
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
			ItemId::None => todo!(),
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
			let block_code = self.cmp_scope(body, definitions)?;
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

		let callable_id = self.context
			.find_item_maybe(&call_expression.name.slice, definitions);

		// Find function id in the current context
		let fn_id =
			if let Some(ItemId::Class(class_id)) = callable_id {
				self.get_class_constructor_id(
					class_id,
					function_identity,
					definitions
				)?
			} else {
				if caller_is_container {
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
							let fn_group = module.common.get_function_group_id(
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
						.get_function_id_by_identity(
							&function_identity,
							&definitions,
						)?
				}
			};

		// Compile the function call
		builder.add_op(QuOp::Call(
			fn_id,
			parameter_ids.into_boxed_slice(),
					store_to
				));
				builder.return_type = definitions
			.get_function(fn_id)?
			.identity
					.return_type;

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
		// Compose indentity and parameters list
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
						let id = definitions.class_id::<Void>()?;
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
				parameters: parameters_types.into_boxed_slice(),
				return_type,
			};
			(identity, parameters_names)
		};

		// Define the function
		let func_id = definitions.functions.len();
		let frame = self.context.frames.iter().rev().next().unwrap();
		let context_id = ItemId::from(frame);
		definitions.define_function_in_item(identity, context_id)?;

		self.context.open_frame(ContextFrame::Function(
			func_id, FrameData::default(),
		));
		let body_code = {
			let mut b = QuAsmBuilder::new();

			// Allocate parameters
			let mut i = 0;
			for (name, class_id) in &parameters {
				let stack_id = self.context.define_variable(
					name.into(),
					*class_id,
					definitions,
				)?.stack_id;
				b.add_op(QuOp::LoadArg(i, stack_id));
				i += 1;
			}

			// Compile code block
			b.add_builder(self.cmp_code_block(&body.code_block, definitions)?);
			Ok::<QuAsmBuilder, QuMsg>(b)
		}?;
		self.context.close_frame();


		// Compile function body
		definitions
			.get_function_mut(func_id)?
			.code_block = FunctionReference::Internal(
				definitions
			.byte_code_blocks
					.len()
			);
		let bytcode = body_code.ops;
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
				ItemId::Function(_) => todo!(),
				ItemId::FunctionGroup(_) => todo!(),
				ItemId::Module(id) => {
					definitions
						.get_module(id)?
						.common
						.get_item_id(indexer)?
				},
				ItemId::StaticVariable(_) => todo!(),
				ItemId::Variable(_) => todo!(),
				ItemId::None => todo!(),
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
				self.context.import_class_or_module(
					ItemId::Class(id),
					None,
					definitions
				)?;
			},
			ItemId::Constant(_) => todo!(),
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
			ItemId::None => todo!(),
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
						// Allocate space for return value.
						let expression_reg = self.context.allocate(
							return_type,
							definitions,
						)?;
						
						code.add_builder(self.cmp_expr(
							expression,
							expression_reg,
							definitions
						)?);

						if expression_reg.0 != return_reg.0 {
							// Copy result to return register if its not
							// already there
							code.add_builder(self.cmp_copy_register(
								expression_reg,
								return_reg,
								definitions,
							)?);
						}

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
		)?.ops;
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
	pub(crate) fn compile(
		&mut self, code:&str, definitions: &mut Definitions,
	) -> Result<Vec<QuOp>, QuMsg> {
		let mut p = QuParser::new();
		let code_block = p.parse(code)?;
		
		let base_id = *definitions.module_map
			.get(FUNDAMENTALS_MODULE)
			.unwrap();

		self.context.open_frame(ContextFrame::module(base_id));

		self.prepass(&code_block, definitions)?;
		let compiled = self.compile_code(&code_block, definitions)?;
		self.context.close_frame();

		Ok(compiled)
	}


	/// Compiles Qu code from a [QuLeaf] into a [`Vec<u8>`].
	pub(crate) fn compile_code(
		&mut self, code_block:&CodeBlock, definitions: &mut Definitions
	) -> Result<Vec<QuOp>, QuMsg> {
		// Main code
		let code = {
			let code = self.cmp_module(code_block, definitions)?;
			Ok::<QuAsmBuilder, QuMsg>(code)
		}?;
		
		Ok(code.ops)
	}


	/// Returns an appropriate location to store an expression.
	/// 
	/// Most expressions require a new memory location, but variables
	/// should just return the register of the variable.
	fn get_expr_reg(
		&mut self,
		expr_leaf:&Expression,
		definitions: &mut Definitions,
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
			Expression::Bool(_) => {
				self.context.allocate(
					definitions.class_id::<Bool>()?,
					definitions,
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
    		Expression::Number(number) => {
				let class_id = match number.decimal {
					Some(_) => Float::get_id(&definitions.uuid).unwrap(),
					None => Int::get_id(&definitions.uuid).unwrap(),
				};
				self.context.allocate(class_id, definitions)
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
			Expression::Bool(_) => {
				Bool::get_id(&definitions.uuid).unwrap()
			},
			Expression::Tuple(_tuple) => {
				unimplemented!()
			},
			Expression::Var(var) => {
				let item = self.context.find_item(
					&var.name.slice,
					definitions,
				)?;

				let id = match item {
					ItemId::Class(_) => {
						definitions.class_id::<Class>()?
					},
					ItemId::Constant(id) =>
						definitions.constants[id].class_id,
					ItemId::Function(_) => todo!(),
					ItemId::FunctionGroup(_) => todo!(),
					ItemId::Module(_) => {
						definitions.class_id::<Module>()?
					},
					ItemId::StaticVariable(_) => todo!(),
					ItemId::Variable(id) => {
						let var = self.context
							.get_variable(id)?;
						var.stack_id.class_id()
					},
					ItemId::None => todo!(),
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
			Expression::Number(number) => {
				let class_id = match number.decimal {
					Some(_) => Float::get_id(&definitions.uuid).unwrap(),
					None => Int::get_id(&definitions.uuid).unwrap(),
				};
				class_id
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
						parameters: parameters.into_boxed_slice(),
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

#[derive(Debug, Default, Clone)]

struct QuAsmBuilder {
	ops: Vec<QuOp>,
	return_type: ClassId,
} impl QuAsmBuilder {
	fn new() -> Self {
		return Self {
			ops: vec![],
			return_type: ClassId::new(0),
		}
	}

	fn add_builder(&mut self, mut builder:QuAsmBuilder) {
		self.ops.append(&mut builder.ops);
	}


	fn add_op(&mut self, op:QuOp) {
		self.ops.push(op);
	}

	fn len(&self) -> usize {
		return self.ops.len();
	}
}