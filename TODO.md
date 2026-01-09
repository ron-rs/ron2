implicit_some extension

  8. Implicit Some Wrapping is Surprising

  In convert.rs, Option<T>::from_ast:

  impl<T: FromRon> FromRon for Option<T> {
      fn from_ast(expr: &Expr<'_>) -> SpannedResult<Self> {
          match expr {
              Expr::Option(opt) => /* handle Some/None */,
              other => Ok(Some(T::from_ast(other)?)),  // Implicit wrap!
          }
      }
  }

  Raw values automatically become Some(value). This means:
  Option::<i32>::from_ron("42")  // Returns Ok(Some(42))
