
use duplicate::duplicate_item;
use once_cell::sync::Lazy;

use crate::Bool;
use crate::Class;
use crate::Float;
use crate::Int;
use crate::Module;
use crate::QuAdd;
use crate::QuDiv;
use crate::QuEqual;
use crate::QuGreater;
use crate::QuLesser;
use crate::QuMod;
use crate::QuMul;
use crate::QuNotEqual;
use crate::QuPow;
use crate::QuSub;
use crate::import::FunctionId;
use crate::objects;
use crate::vm::QuOp;
use crate::vm::QuOp::*;
use crate::QuParser;
use crate::Register;
use crate::TypedRegId;
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
use crate::vm::RegId;

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

	pub const_id: ConstantId,
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


	/// Searches the item's immediate items for an item with the given name
	pub fn get_item_id(&self, identity: &str) -> Result<ItemId, QuMsg> {
		if let Some(id) = self.class_map.get(identity) {
			return Ok(ItemId::Class(*id));
		}
		if let Some(id) = self.constants_map.get(identity) {
			return Ok(ItemId::Constant(*id));
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

	/// Searches all the item's implementations for an item with the given name
	#[duplicate_item(
		get_item_id_deep Return some(value) none(value);
		[get_item_id_deep] [Result<ItemId, QuMsg>] [Ok(value)] [Err(value.into())];
		[get_item_id_deep_maybe] [Option<ItemId>] [Some(value)] [None];
	)]
	pub fn get_item_id_deep(
		&self,
		identity: &str,
		d:&Definitions,
	) -> Return {
		if let Some(id) = self.class_map.get(identity) {
			return some([ItemId::Class(*id)]);
		}
		if let Some(id) = self.constants_map.get(identity) {
			return some([ItemId::Constant(*id)]);
		}
		if let Some(id) = self.function_groups_map.get(identity) {
			return some([ItemId::FunctionGroup(*id)]);
		}
		if let Some(id) = self.static_variables_map.get(identity) {
			return some([ItemId::StaticVariable(*id)]);
		}

		for (trait_id, im) in &self.implementations {
			let trait_class = d.get_class(*trait_id).unwrap();
			let tarit_item_id = trait_class.common
				.get_item_id_deep_maybe(identity, d);
			let Some(item_id) = tarit_item_id
				else {continue};
			if let Some(id) = im.get_implemented_item_maybe(item_id) {
				return some([id]);
			}
		}

		none([format!(
			"Item '{}' has no item by name '{}'.",
			self.name,
			identity,
		)])
	}

} impl Default for CommonItem {
	fn default() -> Self {
		Self {
			name: "".into(),
			const_id: Default::default(),
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
} impl Constant {
	fn get_value<T: Register + 'static>(&self) -> &T {
		assert_eq!(T::size() as usize, self.value.len());
		assert_eq!(self.class_id, T::id());
		let x:*const [u8] = &*self.value;
		unsafe { x.cast::<T>().as_ref().unwrap() }
	}
}

/// The context for what is being compiled. Records things like temporary
/// variables and import shortcuts.
#[derive(Debug, Default, Clone)]
struct Context {
	frames: Vec<ContextFrame>,
	variables: Vec<VariableMetadata>,
} impl Context {
	fn allocate(
		&mut self,
		class:ClassId,
		definitions: &Definitions,
	) -> Result<TypedRegId, QuMsg> {
		match self.get_current_context_frame_mut() {
			ContextFrame::Module(_, frame_data) => {
				let scope = frame_data.scopes.last_mut().unwrap();
				let stack_id = TypedRegId::new(
					scope.stack_size.into(),
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
				let stack_id = TypedRegId::new(
					scope.stack_size.into(),
					class,
				);
				scope.stack_size += definitions
					.get_class(class)?
					.size as usize;
				Ok(stack_id)
			},
		}
	}

	fn allocate_at(
		&mut self,
		reg: RegId,
		class:ClassId,
		definitions: &Definitions,
	) -> Result<TypedRegId, QuMsg> {
		match self.get_current_context_frame_mut() {
			ContextFrame::Module(_, frame_data)
			| ContextFrame::Function(_, frame_data) => {
				let scope = frame_data.scopes.last_mut().unwrap();
				let stack_id = TypedRegId::new(
					scope.stack_size.into(),
					class,
				);
				if scope.stack_size > reg.0 {
					return Err(format!(
						"Allocation at index {} is already allocated",
						reg.0,
					).into());
				}
				scope.stack_size = reg.0 + definitions
					.get_class(class)?
					.size as usize;
				Ok(stack_id)
			},
			ContextFrame::Class(_, _) => {
				/* Class variables are handled sparately, pass */
				panic!()
			},
		}
	}


	/// Returns the next stack index available for allocation
	/// without making any allocations
	fn next_reg(&self) -> RegId {
		match self.get_current_context_frame() {
			ContextFrame::Module(_, frame_data)
			| ContextFrame::Function(_, frame_data) => {
				frame_data.scopes.last().unwrap().stack_size.into()
			},
			ContextFrame::Class(_, _) => {
				/* Class variables are handled sparately, pass */
				panic!()
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


	fn find_function_maybe(
		&self,
		identity: &FunctionIdentity,
		d: &Definitions,
	) -> Option<FunctionId> {
		let item = self.find_in_stack_filtered(
			&identity.name,
			&|found_item| {
				match found_item {
					ItemId::Function(id) =>
						identity.compare(
							&d.get_function(id).unwrap().identity,
							d
						),
					ItemId::FunctionGroup(id) => d
						.get_function_group(id)
						.unwrap()
						.get_fn_id_maybe(identity, d)
						.is_some(),
					_ => false,
				}
			},
		);
		if let Some(function_id) = item {
			return match function_id {
				ItemId::Function(function_id) => {
					Some( function_id )
				},
				ItemId::FunctionGroup(id) => {
					d.function_groups[id]
						.get_fn_id_maybe(&identity, d)
				},
				_ => unreachable!(),
			};
		}

		let item = 'frames: {
			for frame in self.frames.iter().rev() {
				match frame {
					ContextFrame::Module(module_id, _) => {
						let module = d.get_module(
							*module_id
						).unwrap();
						let mut item_opt = None;
						if let Some(group_id) =
							module.common.function_groups_map.get(&identity.name)
						{
							if let Some(some_fn_id) =
								d.function_groups[*group_id]
									.get_fn_id_maybe(identity, d)
							{
								item_opt = Some(some_fn_id)
							}
						}

						if let Some(item) = item_opt {
							break 'frames Some(item);
						}
					},
					ContextFrame::Class(class_id, _) => {
						let class = d.get_class(
							*class_id
						).unwrap();
						let mut item_opt = None;
						if let Some(group_id) =
							class.common.function_groups_map.get(&identity.name)
						{
							if let Some(some_fn_id) =
								d.function_groups[*group_id]
									.get_fn_id_maybe(identity, d)
							{
								item_opt = Some(some_fn_id)
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
				for pair
				in scope.definitions_map
					.iter()
					.rev()
					.filter(|pair| pair.0 == identity)
					.filter(|pair| filter(pair.1))
				{
					return Some(pair.1);
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
	pub classes: HashMap<ClassId, QuStruct>,
	pub functions: Vec<FunctionMetadata>,
	pub function_groups: Vec<FunctionGroup>,
	pub modules: Vec<ModuleMetadata>,

	pub(crate) byte_code_blocks: Vec<Vec<QuOp>>,

	/// A map of names to module IDs.
	pub module_map: HashMap<String, ModuleId>,
	pub private_constants: HashMap<String, ConstantId>,
	/// All registered classes with external functions that were not registered.
	with_unregistered_functions: Vec<ClassId>, // TODO: Remove with_unregistered_functions
} impl Definitions {
	pub fn new(uuid: Uuid) -> Self {
		let mut d = Self::default();
		// TODO: Delete mapping from REGISTERED_BANK upon Definitions being dropped
		d.uuid = uuid;
		REGISTERED_BANK.write().unwrap().insert(uuid, HashMap::default());
		d
	}


	pub fn add_to_function_group(
		&mut self,
		item_id: ItemId,
		fn_id: FunctionId,
	) -> Result<FunctionGroupId, QuMsg> {
		let identity = self.get_function(fn_id)?
			.identity
			.clone();
		let new_group_id = self.function_groups.len();

		fn add_to_group(
			group_id:FunctionGroupId,
			identity:FunctionIdentity,
			fn_id:FunctionId,
			d: &mut Definitions,
		) -> Result<(), QuMsg>{
			let group = d.get_function_group_mut(group_id)?;
			group.map.push((identity.clone(), fn_id));
			Ok(())
		}

		let function_group_map = match item_id {
			ItemId::Class(id) => {
				&mut self.get_class_mut(id)?.common.function_groups_map
			},
			ItemId::Constant(_) => todo!(),
			ItemId::Function(_) => todo!(),
			ItemId::FunctionGroup(id) => {
				add_to_group(id, identity, fn_id, self)?;
				return Ok(id);
			},
			ItemId::Module(id) => {
				&mut self.get_module_mut(id)?.common.function_groups_map
			},
			ItemId::StaticVariable(_) => todo!(),
			ItemId::Variable(_) => todo!(),
			ItemId::None => panic!(),
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

		add_to_group(group_id, identity, fn_id, self)?;

		Ok(group_id)
	}


	pub fn class_id<T:Register>(
		&self
	) -> Result<ClassId, QuMsg> {
		self.get_class(T::id())?;
		Ok(T::id())
	}

	/// Adds a constant without binding any names to it.
	pub fn add_constant<T: Register + 'static>(
		&mut self,
		name: String,
		value: T,
	) -> Result<ConstantId, QuMsg> {
		let class_id = T::id();

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


	pub fn add_function(&mut self, mut function:FunctionMetadata) -> FunctionId {
		let id = self.functions.len().into();
		let const_id = self.add_constant(
			function.identity.name.clone(),
			objects::Function {id},
		).unwrap();
		function.const_id = const_id;
		self.functions.push(function);
		id
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
		let const_id = self.add_constant(
			name.clone(),
			Module {id: module_id},
		)?;

		let mut common = CommonItem::default();
		common.const_id = const_id;
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


	pub fn get_class(&self, id: ClassId) -> Result<&QuStruct, QuMsg> {
		let class = self.classes
			.get(&id)
			.ok_or_else(|| -> QuMsg { format!(
				"There's no class with id '{:?}' defined.", id,
			).into()})?;
		Ok(class)
	}


	pub(crate) fn get_class_mut(
		&mut self, id: ClassId,
	) -> Result<&mut QuStruct, QuMsg> {
		let class = self.classes
			.get_mut(&id)
			.ok_or_else(|| -> QuMsg { format!(
				"There's no class with id '{:?}' defined.", id,
			).into()})?;
		Ok(class)
	}


	pub fn get_function(&self, id: FunctionId) -> Result<&FunctionMetadata, QuMsg> {
		let function = self.functions
			.get(usize::from(id))
			.ok_or_else(|| -> QuMsg { format!(
				"There's no function with id '{:?}' defined.", id,
			).into()})?;
		Ok(function)
	}


	#[duplicate_item(
		get_function_group SelfType FunctionGroupRef get_ref;
		[get_function_group] [&Self] [&FunctionGroup] [get];
		[get_function_group_mut] [&mut Self] [&mut FunctionGroup] [get_mut];
	)]
	pub fn get_function_group(
		self: SelfType,
		id: FunctionGroupId,
	) -> Result<FunctionGroupRef, QuMsg> {
		let group = self.function_groups
			.get_ref(usize::from(id))
			.ok_or_else(|| -> QuMsg { format!(
				"There's no function group with id '{:?}' defined.", id,
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


	fn get_private_constant<T: Register + 'static>(
		&mut self,
		name:&str,
		init:&dyn Fn()-> T,
	) -> ConstantId {
		let id = match self.private_constants.get(name) {
			Some(id) => *id,
			None => {
				self.add_constant(name.into(), (init)())
					.unwrap()
			},
		};
		id
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
			let trait_group = self.get_function_group(group_id)?;
			
			// Create implementation functions
			for (_, fn_id) in &trait_group.map {
				implementation.functions.insert(*fn_id, *fn_id);
			}
			
			// Create implementation function groups
			let impl_group_id = {
				let impl_group_id = self.function_groups.len();
				let impl_group = FunctionGroup {
					name: trait_group.name.clone(),
					map: Default::default(),
				};
				let trait_group_pairs:Vec<(FunctionIdentity, FunctionId)> = trait_group.map
					.iter()
					.map(|pair|
						(pair.0.clone(), pair.1.clone())
					)
					.collect();
				self.function_groups.push(impl_group);
				for (key, fn_id) in trait_group_pairs {
					// Copy over keys and values, converting cases of Self
					// to the class type
					let mut impl_key = key;
					impl_key.set_self_type(class_id);
					self.add_to_function_group(
						ItemId::FunctionGroup(impl_group_id),
						fn_id,
					)?;
				}
				impl_group_id
			};
			implementation.function_groups.insert(group_id, impl_group_id);
		}

		let class_data = self.get_class_mut(class_id)?;
		if class_data.common.implementations.contains_key(&trait_id) {
			return Err(format!(
				"Can't implment trait, {}, in class, {}, because it's already implemented",
				trait_common.name,
				class_data.common.name,
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


	/// The standard way to register a function with Qu.
	/// Handles both external and internal functions
	/// 
	/// # Examples
	/// 
	/// ``` qu
	/// int.add(2, 3) # Valid
	/// 2.add(3) # Valid
	/// add(2, 3) # Valid
	/// ```
	pub fn define_function_in_item(
		&mut self,
		item_id: ItemId,
		mut fn_definition:FunctionMetadata,
		default_class:Option<ClassId>,
		auto_add_to_class:bool,
	) -> Result<FunctionId, QuMsg> {
		let class_id = default_class
			.or_else(||{
				fn_definition
					.identity
					.parameters
					.first()
					.map(|x| *x)
			});
		
		if let Some(x) = class_id { if x.is_self_type() { panic!(
			"Can't define function, {}, because self type is ambiguous",
			fn_definition.identity.display_pretty(self)
		) } }

		// Replace instances of the self type
		match (class_id, auto_add_to_class) {
			(Some(class_id), true) /*if !self.get_class(class_id)?.is_trait*/ => {
				// Replace instances of the self type with the specific class
				fn_definition.identity.set_self_type(class_id);
			},
			(_, false) | (None, _) => {
				// Function is static, throw an error if any Self types exist
				if fn_definition.identity.has_self_type() {
					panic!(
						"Couldn't define static function, {}, because it contains a Self type, which can only be used in non-static functions.",
						fn_definition.identity.display_pretty(self)
					)
				}
			}
		}

		// Register function
		let new_function_id = self.add_function(fn_definition);

		// Also register function with module.
		match class_id {
			Some(class_id) if auto_add_to_class => {
				self.add_to_function_group(
					ItemId::Class(class_id),
					new_function_id
				)?;
			},
			_ => {},
		}
		self.add_to_function_group(
			item_id,
			new_function_id
		)?;

		Ok(new_function_id)
	}


	pub fn define_function_implementation(
		&mut self,
		class_id:ClassId,
		trait_id:ClassId,
		parent_item:ItemId,
		mut external_function:FunctionMetadata,
	) -> Result<(), QuMsg> {
		// Convert cases of Self to the implementing class
		external_function.identity.set_self_type(class_id);

		let trait_group_id = self.get_class(trait_id)?
			.common
			.get_function_group_id(
				&external_function.identity.name
			)?;
		let trait_group = self.get_function_group(
			trait_group_id
		)?;
		let trait_fn_id = trait_group
			.get_fn_id(&external_function.identity, self)?;
		let trait_fn = self.get_function(trait_fn_id)?;
		if !external_function.identity.return_type.is(
			trait_fn
				.identity
				.return_type,
			self
		) {
			panic!(
				"Couldn't implment function, {2}, in class, {0}, for trait, {1}, because the trait's function signature, {1}.{3}, doesn't match.",
				self.get_class(class_id)?.common.name,
				self.get_class(trait_id)?.common.name,
				external_function.identity.display_pretty(self),
				trait_fn.identity.display_pretty(self),
			)
		}
		
		let impl_fn_id = self.add_function(external_function);

		// Add to class's and module/parent's function groups
		let impl_group_id = self.add_to_function_group(
			ItemId::Class(class_id),
			impl_fn_id,
		)?;
		self.add_to_function_group(
			parent_item,
			impl_fn_id,
		)?;
		
		// Override old function and group implementation mappings
		let implementations = self.get_class_mut(class_id)?
			.common
			.get_trait_implementation_mut(trait_id)?;
		implementations.functions.insert(trait_fn_id, impl_fn_id);
		implementations.function_groups.insert(trait_group_id, impl_group_id);

		Ok(())
	}


	/// Registers an external struct to be used within the Qu langauge.
	pub fn register_module_struct<T: Register + 'static>(
		&mut self,
		module_id: ModuleId,
	) -> Result<ClassId, QuMsg> {
		let class_name = <T as Register>::name();

		// Manage classes map
		let class_id:ClassId = T::id();
		let module = self.get_module_mut(module_id)?;
		assert!(!module.common.class_map.contains_key(class_name));
		module.common.class_map.insert(class_name.into(), class_id);

		
		// Add struct to register bank
		REGISTERED_BANK
			.write()
			.unwrap()
			.get_mut(&self.uuid)
			.unwrap()
			.insert(TypeId::of::<T>(), class_id);
	
		let const_id = self.add_constant(
			class_name.into(),
			Class { id: class_id },
		)?;
	
		// Add class to list of classes
		let mut class = QuStruct::new(
			class_name,
			size_of::<T>(),
		);
		class.common.const_id = const_id;
		self.classes.insert(class_id, class);

		Ok(class_id)

	}
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
	map: Vec<(FunctionIdentity, FunctionId)>,
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
		d:&Definitions,
	) -> Result<FunctionId, QuMsg> {
		let Some(fn_id) = self.get_fn_id_maybe(by_identity, d)
			else { return Err(format!(
				"Couldn't find function, {}, in group.",
				by_identity.display_pretty(d),
			).into())};
		Ok(fn_id)
	}
	
	fn get_fn_id_maybe(
		&self,
		by_identity:&FunctionIdentity,
		d:&Definitions,
	) -> Option<FunctionId> {
		let mut matched: Option<(FunctionId, &FunctionIdentity)> = None;
		for (identity, fn_id) in self.map.iter().rev() {
			if !by_identity.compare(identity, d) {
				continue;
			}
			if let Some(_) = matched {
				if by_identity.parameters.first() != identity.parameters.first() {
					// This iteration's identity is a no better match, skip
					continue
				}
			}

			matched = Some((*fn_id, identity));
			if by_identity.parameters.first() == identity.parameters.first() {
				// Found best match
				break
			}
		}
		matched.map(|x|{x.0})
	}
}

pub type FunctionGroupId = usize;


#[derive(Clone, Debug, Default, Eq, Ord)]
pub struct FunctionIdentity {
	pub name: String,
	pub parameters: Box<[ClassId]>,
	pub return_type: ClassId,
} impl FunctionIdentity {
	fn compare(&self, other:&Self, d:&Definitions) -> bool {
		if self.name != other.name {
			return false;
		}
		if self.parameters.len() != other.parameters.len() {
			return false;
		}

		for (s_id, o_id) in self.parameters.iter().zip(other.parameters.iter()) {
			if !s_id.is(*o_id, d) {
				return false;
			}
		}

		return true;
	}

	fn display_pretty(&self, d:&Definitions) -> String {
		let mut args = "".to_owned();
		for (i, arg) in self.parameters.iter().enumerate() {
			args.push_str(
				d.get_class(*arg).unwrap().common.name.as_str()
			);
			if i < self.parameters.len() - 1 {
				args.push_str(", ")
			}
		}
		let ret_type = d.get_class(self.return_type).unwrap().common.name.as_str();

		format!(
			"{0}({1}) -> {2}",
			self.name,
			args,
			ret_type
		)
	}

	fn has_self_type(&mut self) -> bool{
		self.return_type.is_self_type()
		|| {
			for arg in self.parameters.iter() {
				if arg.is_self_type() {
					return true;
				}
			}
			false
		}
	}

	fn set_self_type(&mut self, to_type:ClassId) {
		self.return_type.set_self(to_type);
		for arg in self.parameters.iter_mut() {
			arg.set_self(to_type);
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
		write!(f,"{}({})->{}", self.name, params, self.return_type.0)
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
	pub const_id: ConstantId,
}

#[derive(Debug, Default, Clone)]
pub(crate) struct Implementation {
	functions: HashMap<FunctionId, FunctionId>,
	function_groups: HashMap<FunctionGroupId, FunctionGroupId>,
} impl Implementation {
	fn get_implemented_item(&self, item_id:ItemId) -> Result<ItemId, QuMsg> {
		if let Some(id) = self.get_implemented_item_maybe(item_id) {
			return Ok(id);
		}
		Err(format!(
			"Failed to get implemented item for id {:?}. TODO: better msg",
			item_id,
		).into())
	}

	fn get_implemented_item_maybe(&self, item_id:ItemId) -> Option<ItemId> {
		if let ItemId::Function(id) = item_id {
			return self.functions
				.get(&id)
				.map(|x| ItemId::Function(*x));
		}
		if let ItemId::FunctionGroup(id) = item_id {
			return self.function_groups
				.get(&id)
				.map(|x| ItemId::FunctionGroup(*x));
		}
		None
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
	definitions_map: Vec<(String, ItemId)>,
	/// The number of bytes that have been allocated to the stack in this scope.
	stack_size: usize,
} impl Scope {
	fn define_variable(
		&mut self,
		name: String,
		id: VariableId,
	) -> Result<(), QuMsg> {
		// TODO: Do something about variable shadowing
		self.definitions_map.push((
			name,
			ItemId::Variable(id),
		));
		Ok(())
	}


	fn find_item(
		&self,
		name: &str,
	) -> Option<ItemId> {
		for pair in self.definitions_map.iter().rev() {
			if name == pair.0 {
				return Some(pair.1)
			}
		}
		None
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
				let item_name = match id {
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
				item_name
			},
		};

		self.definitions_map.push((
			name,
			id,

		));
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
			self.definitions_map.push((
				group_name.clone(),
				ItemId::FunctionGroup(*group_id),
			));
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

		self.definitions_map.push((
			name,
			ItemId::FunctionGroup(id),
		));

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

		self.definitions_map.push((
			name,
			ItemId::Module(module_id),
		));

		Ok(())
    }
}


type VariableId = usize;


#[derive(Debug, Default, Clone)]
struct VariableMetadata {
	stack_id: TypedRegId,
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


	fn cmp_alloc_expression(
		&mut self,
		expression:&Expression,
		definitions:&mut Definitions,
	) -> Result<QuAsmBuilder, QuMsg>{
		let reg = self.get_expr_reg(expression, definitions)?;
		let needs_allocating = reg == self.context.next_reg();
		let b = self.cmp_expr(
			expression,
			reg,
			None,
			definitions,
		)?;
		assert_eq!(reg, b.return_reg.index());
		if needs_allocating {
			self.context.allocate_at(
				b.return_reg.index(),
				b.return_reg.class_id(),
				definitions,
			)?;
		}
		Ok(b)
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


	/// Compiles a copy from one register to another.
	fn cmp_copy_register(
		&self,
		from: TypedRegId,
		to: RegId,
		definitions: &mut Definitions
	) -> Result<QuAsmBuilder, QuMsg>{
		let identity = &FunctionIdentity {
			name: "copy".into(),
			parameters: Box::new([from.class_id()]),
			return_type: Default::default(),
		};
		
		let group_id = definitions.get_class(from.class_id())?
			.common
			.get_function_group_id(&identity.name)?;
		let fn_id = definitions
			.get_function_group(group_id)?
			.get_fn_id(&identity, &definitions)?;
		let fn_data = definitions.get_function(fn_id)?;

		let mut b: QuAsmBuilder = QuAsmBuilder::new();
		b.add_op(QuOp::Call(
			fn_id,
			Box::new([from.into()]),
			to.into(),
		));
		b.return_reg.1 = fn_data.identity.return_type;

		Ok(b)
	}


	/// Compiles an expression into bytecode.
	fn cmp_expr(
		&mut self,
		expression:&Expression,
		reg: RegId,
		reg_type: Option<ClassId>,
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
					reg,
					definitions,
				)
			}
			Expression::DotIndex(
				dot_index,
			) => self.cmp_expr_dot_index(
				dot_index,
				reg,
				definitions,
			),
			Expression::Operation(
				operation_expression,
			) => self.cmp_expr_operation(
				&operation_expression.operator,
				&operation_expression.left,
				&operation_expression.right,
				reg,
				definitions,
			),
			Expression::Bool(
				bool_literal,
			) => self.cmp_expr_bool(&bool_literal.value, reg, definitions),
			Expression::Tuple(
				tuple,
			) => self.cmp_expr_tuple(
					&tuple.elements,
					reg,
					definitions,
				),
			Expression::Var(
				var_expression,
			) => self.cmp_expr_var(
					&var_expression,
					reg,
					definitions,
				),
			Expression::Number(number) =>
				self.cmp_expr_number(
					&number.value,
					&number.decimal,
					reg,
					reg_type,
					definitions,
				),
    		Expression::As(as_expr) => {
				self.context.open_scope();
				let mut b = self.cmp_alloc_expression(
					&as_expr.left,
					definitions,
				)?;
				let ItemId::Class(class_id) = self.context
					.find_item(&as_expr.right.slice, definitions)?
					else {
						todo!("Add err msg");
					};
				b.as_type = Some(class_id);
				self.context.close_scope();
				Ok(b)
				
			},
			}?;

		// Type check
		let void_id = definitions.class_id::<Void>()?;
		if builder.return_reg.class_id() != void_id {
			if let Some(reg_type) = reg_type {
				if builder.return_reg.class_id() != reg_type {
					return Err(format!(
						"Attempted to assign a value of type '{}' to a location of type '{}'.",
						definitions.get_class(builder.return_reg.class_id())?
							.common.name,
						definitions.get_class(reg_type)?.common.name,
					).into());
				}
			}
		}

		Ok(builder)
	}


	/// Compiles a dot index. (Ex: foo.bar).
	fn cmp_expr_dot_index(
		&mut self,
		dot_index: &DotIndex,
		reg: RegId,
		definitions: &mut Definitions,
	)-> Result<QuAsmBuilder, QuMsg> {
		let b_left = self.cmp_alloc_expression(
			&dot_index.left,
			definitions,
		)?;
		let left_class = definitions.get_class(
			b_left.return_reg.class_id()
		)?;
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
					reg,
					definitions
				)?
			},
			ItemId::None => todo!(),
		};

		Ok(builder)
	}


	fn asm_expr_operation(
		&mut self,
		operator: QuOperator,
		left: QuAsmBuilder,
		right: QuAsmBuilder,
		// TODO: Change output_reg to a struct without type information
		reg: RegId,
		reg_type: Option<ClassId>,
		definitions: &mut Definitions,
	)-> Result<QuAsmBuilder, QuMsg> {
		self.context.open_scope();
		let trait_id = match operator {
			QuOperator::Add => QuAdd::id(),
			QuOperator::Sub => QuSub::id(),
			QuOperator::Mul => QuMul::id(),
			QuOperator::Div => QuDiv::id(),
			QuOperator::Mod => QuMod::id(),
			QuOperator::Pow => QuPow::id(),
			QuOperator::Sqrt => todo!(),
			QuOperator::Less => QuLesser::id(),
			QuOperator::LessEq => todo!(),
			QuOperator::Great => QuGreater::id(),
			QuOperator::GreatEq => todo!(),
			QuOperator::Eq => QuEqual::id(),
			QuOperator::NotEq => QuNotEqual::id(),
			QuOperator::And => todo!(),
			QuOperator::Or => todo!(),
			QuOperator::Is => {
				let left_is_dynamic = definitions
					.get_class(left.return_reg.class_id())?
					.is_trait;

				if right.return_reg.class_id() != Class::id() {
					return Err(format!(
						"The 'is' expression expected right to be a type, but it's a {}",
						definitions
							.get_class(right.return_reg.class_id())?
							.common.name
					).into())
				}

				let b = match (left_is_dynamic, &right) {
					(
						false,
						QuAsmBuilder{constant: Some(const_id), ..}
					) => {
						// Types are known at compile-time, optimize to constant
						let class = definitions.constants[*const_id]
							.get_value::<Class>();
						let const_id = match
						left.return_reg.class_id().is(class.id, &definitions)
						{
							true => definitions
								.get_private_constant(
									"true",
									&||{true},
								),
							false => definitions
								.get_private_constant(
									"false",
									&||{false},
								),
						};
						let mut builder = QuAsmBuilder::new();
						builder.add_builder(left);
						builder.add_builder(right);
						builder.add_return_op(
							QuOp::LoadConstant(const_id, reg),
							definitions.class_id::<Bool>()?,
						);
						builder
					}
					(false, _) => {
						// Type to check against is not known at compile-time,
						// do a runtime check
						let left_type_const = definitions
							.get_class(left.return_reg.class_id())?
							.common
							.const_id;
						let left_output = self.context
							.allocate(Class::id(), definitions)?
							.index();
						self.asm_expr_operation(
							QuOperator::Eq,
							QuAsmBuilder::from_const(
								left_type_const,
								left_output,
								&definitions,
							),
							right,
							reg,
							reg_type,
							definitions,
						)?
					}
					_ => todo!("Handle comparisons with dunamic left expression")
				};
				return Ok(b);
			},
		};
		
		let sig = FunctionIdentity {
			name: operator.name().into(),
			parameters: Box::new([
				left.return_reg.class_id(),
				right.return_reg.class_id(),
			]),
			..Default::default()
		};

		let trait_data = definitions.get_class(trait_id)?;
		let trait_group_id = trait_data
			.get_function_group_id(operator.name())?;
		let trait_fn_id = definitions
			.get_function_group(trait_group_id)?
			.get_fn_id(&sig, definitions)?;

		let left_data = definitions.get_class(left.return_reg.class_id())?;
		let ItemId::Function(fn_id) = left_data
			.common
			.get_trait_implementation(trait_id)?
			.get_implemented_item(ItemId::Function(trait_fn_id))?
			else {unreachable!()};
		let fn_data = definitions.get_function(fn_id)?;

		self.context.close_scope();
		self.asm_fn_call(
			None,
			QuAsmBuilder::from_const(
				fn_data.const_id,
				0.into(),
				&definitions,
			),
			vec!(left, right),
			reg,
			definitions,
		)
	}


	/// Compiles a math or logic expression into bytecode.
	fn cmp_expr_operation(
		&mut self,
		operator: &QuToken,
		left: &Expression,
		right: &Expression,
		// TODO: Change output_reg to a struct without type information
		reg: RegId,
		definitions: &mut Definitions,
	)-> Result<QuAsmBuilder, QuMsg> {

		self.context.open_scope();

		let operator = QuOperator::from_symbol(&operator.slice);
		let b_left = self.cmp_alloc_expression(
			left,
			definitions,
		)?;
		let b_right = self.cmp_alloc_expression(
			right,
			definitions,
		)?;

		self.context.close_scope();

		self.asm_expr_operation(
			operator,
			b_left,
			b_right,
			reg,
			None,
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
		reg: RegId,
		definitions: &mut Definitions,
	) -> Result<QuAsmBuilder, QuMsg> {
		let b = match value {
			x if x == KEYWORD_BOOL_TRUE => {
				QuAsmBuilder::from_const(
					definitions.get_private_constant(
						&"true", &||{true}
					),
					reg,
					&definitions,
				)
			},
			x if x == KEYWORD_BOOL_FALSE => {
				QuAsmBuilder::from_const(
					definitions.get_private_constant(
						&"false", &||{false}
					),
					reg,
					&definitions,
				)
			}
			_ => unreachable!()
		};

		Ok(b)
	}


	fn cmp_expr_number(
		// TODO: Change output_reg to a struct without type information
		&mut self,
		value:&QuToken,
		decimal:&Option<QuToken>,
		reg:RegId,
		reg_type: Option<ClassId>,
		definitions: &mut Definitions,
	) -> Result<QuAsmBuilder, QuMsg> {
		let is_flaot = match reg_type {
			Some(reg_type) =>
				reg_type == definitions.class_id::<Float>()?,
			None => decimal.is_some(),
		};

		let cosnt_id = match is_flaot {
			false => {
				let const_id = definitions.get_private_constant(
					&value.slice,
					&||{value.slice.parse::<Int>().expect(&format!(
						"Could not convert text '{}' to int!",
						value.slice
					))}
				);
				const_id
			},
			true => {
				let float_full = format!(
					"{}.{}",
					value.slice,
					match decimal {
						Some(tk) => &tk.slice,
						None => ""
					},
				);
				let const_id = definitions.get_private_constant(
					&float_full,
					&||{float_full.parse::<Float>().expect(&format!(
						"Could not convert text '{}' to float!",
						float_full
					))}
				);
				const_id
			},
		};

		return Ok(QuAsmBuilder::from_const(
			cosnt_id,
			reg,
			&definitions,
		));
	}


	/// Compiles a tuple construction into bytecode.
	fn cmp_expr_tuple(
		&mut self,
		_elements:&Vec<Expression>,
		_reg:RegId,
		_definitions: &mut Definitions,
	) -> Result<QuAsmBuilder, QuMsg> {
		todo!("Add tuples");
	}


	/// Compiles a variable-expression into bytecode.
	fn cmp_expr_var(
		&mut self,
		var_expression: &VarExpression,
		reg: RegId,
		definitions: &mut Definitions,
	) -> Result<QuAsmBuilder, QuMsg> {

		// TODO: Error handling

		let item = self.context.find_item(
			&var_expression.name.slice,
			definitions,
		)?;

		let builder = match item {
			ItemId::Class(id) => {
				let builder = QuAsmBuilder::from_const(
					definitions.get_class(id)?.common.const_id,
					reg,
					&definitions
				);
				builder
			},
			ItemId::Constant(id) => {
				let mut builder = QuAsmBuilder::new();
				builder.add_return_op(
					QuOp::LoadConstant(id, reg),
					definitions.constants[id].class_id,
				);
				builder
			},
			ItemId::Function(_) => todo!(),
			ItemId::FunctionGroup(_) => todo!(),
			ItemId::Module(id) => {
				let builder = QuAsmBuilder::from_const(
					definitions.get_module(id)?.common.const_id,
					reg,
					&definitions
				);
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
				
				if variable.stack_id.0 == reg {
					// Output and variable locations are the same, do nothing
					let mut b = QuAsmBuilder::default();
					b.set_output(
						reg,
						variable.stack_id.class_id(),
					);
					return Ok(b);
				}
		
				self.cmp_copy_register(
					variable.stack_id,
					reg,
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
		condition:&Expression,
		body:&CodeScope,
		definitions: &mut Definitions,
	) -> Result<QuAsmBuilder, QuMsg> {
		// Get expression register
		self.context.open_scope();
		let mut b_cond = self.cmp_alloc_expression(
			condition,
			definitions,
		)?;
		// TODO: Allow conversion of other types to bools in condition
		assert_eq!(b_cond.return_reg.class_id(), definitions.class_id::<Bool>()?);
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

		b_cond.add_builder(b?);

		return Ok(b_cond);
	}


	/// Compiles a *while* statement into bytecode.
	fn cmp_flow_while(
		&mut self,
		condition:&Expression,
		body:&CodeScope,
		definitions: &mut Definitions,
	) -> Result<QuAsmBuilder, QuMsg> {
		// Get expression register
		self.context.open_scope();
		let b_cond = {
			// Expression code
			self.cmp_alloc_expression(condition,definitions)?
		};
		// TODO: Allow conversion of other types to bools in condition
		assert_eq!(b_cond.return_reg.class_id(), definitions.class_id::<Bool>()?);
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
			let expr_code_len = b_cond.len();
			b.add_builder(b_cond);

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


	fn asm_fn_call(
		&mut self,
		caller: Option<QuAsmBuilder>,
		callable: QuAsmBuilder,
		args: Vec<QuAsmBuilder>,
		reg: RegId,
		d: &mut Definitions,
	) -> Result<QuAsmBuilder, QuMsg> {
		let mut builder = QuAsmBuilder::new();

		let Some(callable_const_id) = callable.constant
			else { todo!("Call runtime functions") };
		let fn_id = d.constants[callable_const_id]
			.get_value::<objects::Function>().id;
		let param_regs:Vec<RegId> = args.iter().map(|b| {
			b.return_reg.index()
		}).collect();

		// Caller
		if let Some(caller) = caller {
			if caller.constant.is_none() {
				builder.add_builder(caller);
			}
		}

		// Callable
		if callable.constant.is_none() {
			builder.add_builder(callable);
		}

		// Params
		for arg in args {
			builder.add_builder(arg);
		}

		// Call
		builder.add_return_op(
			QuOp::Call(
				fn_id,
				param_regs.into_boxed_slice(),
				reg,
			),
			d
				.get_function(fn_id)?
				.identity
				.return_type,
		);

		Ok(builder)
	}


	fn cmp_fn_call(
		&mut self,
		call_expression: &CallExpression,
		reg: RegId,
		d: &mut Definitions,
	) -> Result<QuAsmBuilder, QuMsg> {
		self.context.open_scope();

		let b_caller = match &call_expression.caller {
			Some(expression) =>
				Some(self.cmp_alloc_expression(expression, d)?),
			None => None,
		};

		let mut b_args = vec!();

		// Compiler caller
		let b_caller = match b_caller {
			Some(b_caller
				@ QuAsmBuilder{return_reg: TypedRegId(_, type_id), ..}
			) if type_id != Class::id() && type_id != Module::id() => {
				// Caller is part of the call, add it to parameters
				b_args.push(b_caller);
				None
			},
			None =>
				// There is no caller
				None,
			b_caller =>
				// Caller is of type module or class, return it as is
				b_caller,
		};

		// Compiler parameters
		for expression in &call_expression.parameters.elements {
			b_args.push(
				self.cmp_alloc_expression(&expression, d)?
			);
		}

		let mut sig = FunctionIdentity {
			name: call_expression.name.slice.clone(),
			parameters: b_args.iter()
				.map(|b| b.return_reg.class_id())
				.collect(),
			..Default::default()
		};

		fn compile_fn_from_item(
			callable_id:ItemId,
			sig:&FunctionIdentity,
			d:&Definitions,
		) -> Result<QuAsmBuilder, QuMsg> {
			match callable_id {
				ItemId::FunctionGroup(id) => {
					let fn_id = d.get_function_group(id)?
						.get_fn_id(&sig, d)?;
					compile_fn_from_item(ItemId::Function(fn_id), &sig, d)
				},
				ItemId::Function(id) => {
					let func = d.get_function(id)?;
					Ok(QuAsmBuilder::from_const(
						func.const_id,
						0.into(),
						d,
					))
				},
				ItemId::Constant(_) => todo!("Call lambda/callable"),
				ItemId::StaticVariable(_) => todo!("Call lambda/callable"),
				ItemId::Variable(_) => todo!("Call lambda/callable"),
				ItemId::Class(id) => {
					let constructor_sig = FunctionIdentity {
						name: CONSTRUCTOR_NAME.into(),
						parameters: sig.parameters.clone(),
						..Default::default()
					};
					let group_id = d.get_class(id)?
						.common
						.get_function_group_id(&constructor_sig.name)?;
					let fn_id = d.get_function_group(group_id)?
						.get_fn_id(&constructor_sig, d)?;
					compile_fn_from_item(ItemId::Function(fn_id), &sig, d)
				},
				ItemId::Module(_) => todo!("Throw an error"),
				ItemId::None => unreachable!(),
			}
		}

		// Callable
		let b_first_arg = b_args.first();
		let b_callable = match (&b_caller, b_first_arg) {
			(Some(b_caller), _) => {
				// Caller is of type class or module, get function from caller
				let Some(caller_const_id) = b_caller.constant
					else {todo!()};
				let caller_const = &d.constants[caller_const_id];

				let caller_common = if caller_const.class_id == Class::id() {
					&d
						.get_class(caller_const.get_value::<Class>().id)?
						.common
				} else if caller_const.class_id == Module::id() {
						&d
							.get_module(caller_const.get_value::<Module>().id)?
							.common
				} else {unreachable!()};

				let callable_id = caller_common
					.get_item_id_deep(&sig.name, d)?;

				compile_fn_from_item(callable_id, &sig, d)?
			},
			(_, Some(b_first_arg)) => {
				// Find function from context, or first parameter
				if let Some(first_arg_as) = b_first_arg.as_type {
					// Convert signature to bias toward the 'as' type of
					// the first argument
					sig.parameters[0] = first_arg_as;
				}

				let context_callable_id = self.context
					.find_function_maybe(&sig, d)
					.map(|id| ItemId::Function(id));

				let callable_item_id = match (
					context_callable_id,
					b_first_arg.as_type,
				) {
					(
						Some(context_callable_id),
						Some(first_arg_as),
					) => {
						// Get function imlpmented in 'as' trait
						d.get_class(b_first_arg.return_reg.class_id())?
							.common
							.get_trait_implementation(first_arg_as)?
							.get_implemented_item(context_callable_id)?
					},
					(Some(context_callable_id), None) => {
						// Return function found by context
						context_callable_id
					}
					(None, _) => {
						// Function not found in context, look for any
						// item in context
						self.context.find_item(
							&sig.name,
							d,
						)?
					},
				};

				compile_fn_from_item(callable_item_id, &sig, d)?
			},
			_ => {
				let item = self.context.find_item(
					&sig.name,
					d,
				)?;
				compile_fn_from_item(item, &sig, d)?
			}
		};

		self.context.close_scope();

		self.asm_fn_call(
			b_caller,
			b_callable,
			b_args,
			reg,
			d,
		)
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
						let item_id = self.context.find_item_filtered(
							&identity.slice,
							&|item| {
								let ItemId::Class(_) = item
									else {return false;};
								return true;
							},
							definitions,
						)?;
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
		let frame = self.context.frames.iter().rev().next().unwrap();
		let context_id = ItemId::from(frame);
		let func_id = definitions.define_function_in_item(
			context_id,
			FunctionMetadata {
				identity: identity,
				code_block: FunctionReference::Internal(
					definitions.byte_code_blocks.len()
				),
				..Default::default()
			},
			None,
			true,
		)?;

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
				b.add_op(QuOp::LoadArg(i, stack_id.into()));
				i += 1;
			}

			// Compile code block
			b.add_builder(self.cmp_code_block(&body.code_block, definitions)?);
			Ok::<QuAsmBuilder, QuMsg>(b)
		}?;
		self.context.close_frame();


		// Compile function body
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
						.common
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
					let b = self.cmp_alloc_expression(
						expression,
						definitions,
					)?;
					Ok(b)
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
				let code = match &return_statement.value {
					Some(expression) => {
						let mut b = self.cmp_expr(
							expression,
							0.into(),
							None,
							definitions
						)?;
						let type_id = b.return_reg.class_id();

						if self.context.frames.len() == 1 {
							b.add_op(Return(type_id));
						} else {
							b.add_op(End);
						}
						b
					},
					None => QuAsmBuilder::new(),
				};
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
			variable.stack_id.index(),
			Some(variable.stack_id.class_id()),
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
					var_stack_id.index(),
					Some(var_stack_id.class_id()),
					definitions,
				),

			None => {
				// No default value, compile fallback to zero
				// TODO: Attempt to call constructor of object instead of defaulting to 0
				Ok(QuAsmBuilder::from_const(
					definitions.get_private_constant(
						"0".into(),
						&||{0},
					),
					var_stack_id.index(),
					definitions,
				))
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

		const PRINT_TREE:bool = false;
		if PRINT_TREE {dbg!(&code_block);}
		
		let base_id = *definitions.module_map
			.get(FUNDAMENTALS_MODULE)
			.unwrap();

		self.context.open_frame(ContextFrame::module(base_id));

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
	) -> Result<RegId, QuMsg> {
		return match expr_leaf {
			Expression::Operation(_)
			| Expression::Call(_)
			| Expression::Bool(_)
			| Expression::Number(_)
			=> {
				Ok(self.context.next_reg())
			},
			Expression::Var(var) => {
				let item = self.context.find_item(
					&var.name.slice,
					definitions,
				)?;
				if let ItemId::Variable(id) = item {
					let var = self.context.get_variable(id)?;
					Ok(var.stack_id.into())
				} else {
					Ok(self.context.next_reg())
				}
			}
			Expression::Tuple(_) => todo!(),
			Expression::DotIndex(_) => todo!(),
			Expression::As(expr) =>
				self.get_expr_reg(&expr.left, definitions),
		};
	}
}

#[derive(Debug, Default, Clone)]

struct QuAsmBuilder {
	ops: Vec<QuOp>,
	return_reg: TypedRegId,
	as_type: Option<ClassId>,
	constant: Option<ConstantId>,
} impl QuAsmBuilder {
	fn new() -> Self {
		return Self {
			ops: vec![],
			return_reg: Default::default(),
			as_type: None,
			constant: None,
		}
	}

	fn from_const(
		const_id:ConstantId,
		output:RegId,
		def:&Definitions,
	) -> Self {
		let mut b = Self::new();
		b.constant = Some(const_id);
		let const_data = &def.constants[const_id];
		b.add_return_op(
			QuOp::LoadConstant(const_id, output),
			const_data.class_id,
		);
		b
	}

	fn add_builder(&mut self, mut builder:QuAsmBuilder) {
		self.ops.append(&mut builder.ops);
	}

	fn add_op(&mut self, op:QuOp) {
		self.ops.push(op);
	}

	fn add_return_op(&mut self, op:QuOp, type_id:ClassId) {
		self.return_reg.0 = op.get_output();
		self.return_reg.1 = type_id;
		self.add_op(op);
	}

	fn set_output(&mut self, output:RegId, type_id:ClassId) {
		self.return_reg = TypedRegId::new(output, type_id);
	}

	fn len(&self) -> usize {
		return self.ops.len();
	}
}