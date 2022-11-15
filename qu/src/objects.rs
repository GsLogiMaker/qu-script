
//! Defines all types and objects used by Qu.

/// Defines all the types supported by Qu.
#[derive(Debug, Default, Clone)]
pub enum QuType {
	#[default] Void,
	Int,
	Bool,
	String,
	Tuple(Vec<QuType>),
	Array,
	Dictionary,
	Object(usize),
}


/// Defines a block of code.
/// 
/// Often used for Qu functions.
#[derive(Debug, Default, Clone)]
pub struct QuCodeObject {
	/// Where the [`QuCodeObject`] starts. The [`QuCodeObject`] ends at the
	/// `END` VM instruction.
	pub start_index:usize,
} impl QuCodeObject {

	/// Constructs a new [`QuCodeObject`].
	/// 
	/// # Examples
	/// 
	/// ```
	/// use qu::QuCodeObject;
	/// 
	/// let codeobj = QuCodeObject::new(0);
	/// ```
	pub fn new(start_index:usize) -> Self {
		return Self{start_index};
	}

}


/// Defines a Qu function.
#[derive(Debug, Default, Clone)]
pub struct QuFnObject {
	/// The input parameters of the function.
	pub parameters:Vec<QuType>,
	/// The function body.
	pub body:QuCodeObject,
	/// The variable type that this function returns.
	pub return_type:QuType,

} impl QuFnObject {

	/// Constructs a [`QuFnObject`].
	/// 
	/// # Examples
	/// 
	/// ```
	/// use qu::QuCodeObject;
	/// use qu::QuFnObject;
	/// use qu::QuType;
	/// 
	/// let func = QuFnObject::new(
	/// 	vec![],
	/// 	QuCodeObject::new(0),
	/// 	QuType::Void
	/// );
	/// ```
	pub fn new(parameters:Vec<QuType>, body:QuCodeObject, return_type:QuType
	) -> Self {
		return Self{
			parameters,
			body,
			return_type,
		}
	}

}


/// Defines all the types supported by Qu.
#[derive(Debug, Default, Clone)]
pub enum QuValue {
	#[default] Void,
	Int(isize),
	Bool(bool),
	Object(usize),
}


#[cfg(test)]
mod test_objects {

}