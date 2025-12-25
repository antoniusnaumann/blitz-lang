use crate::Operator;

impl Operator {
    /// Returns the binding power (precedence) of the operator.
    /// Higher values bind more tightly.
    /// Returns (left_binding_power, right_binding_power) for infix operators.
    pub fn precedence(&self) -> (u8, u8) {
        match self {
            Operator::Else => (2, 3),
            Operator::Or => (7, 8),
            Operator::And => (10, 11),
            // [Space reserved for bitwise OR: |  (~14-15)]
            // [Space reserved for bitwise XOR: ^  (~17-18)]
            // [Space reserved for bitwise AND: &  (~20-21)]
            Operator::Eq | Operator::Ne => (24, 25),
            Operator::Lt | Operator::Le | Operator::Gt | Operator::Ge => (24, 25),
            // [Space reserved for 'in', 'is' keyword operators (~27-28)]
            // [Space reserved for bitwise shift: <<, >>  (~31-32)]
            Operator::Concat => (35, 36),
            // [Space reserved for range operators: .., ...  (~38-39)]
            Operator::Add | Operator::Sub => (42, 43),
            Operator::Mul | Operator::Div | Operator::Rem => (45, 46),
            // [Space reserved for power operator ** (right-associative): (~50, 49)]
            Operator::Not | Operator::Neg => (0, 53),
            // [Space reserved for 'as' type casting (~55-56)]
            Operator::Member => (59, 60),
            // Scope resolution :: - binds stronger than . for UFCS like `matrix.my_matrix_lib::transpose()`
            // TODO: scope resolution
            // [Space reserved for :: (~62-63)]
        }
    }

    /// Returns true if this is a prefix (unary) operator
    pub fn is_prefix(&self) -> bool {
        matches!(self, Operator::Not | Operator::Neg)
    }

    /// Returns true if this is an infix (binary) operator
    pub fn is_infix(&self) -> bool {
        !self.is_prefix()
    }
}
