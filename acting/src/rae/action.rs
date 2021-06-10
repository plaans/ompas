pub type ActionId = usize;

pub type Primitive = String; //Lisp code;

pub struct Action {
    pub primitive: Primitive,
    pub id: ActionId,
}


