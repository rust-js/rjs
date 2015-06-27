use syntax::ast::*;

pub trait AstVisitor<'a> {
    fn visit_function(&mut self, function: &'a Function) {
        self.visit_root_block(&function.block);
    }
    
    fn visit_root_block(&mut self, block: &'a RootBlock) {
        self.visit_block(&block.block);
    }
    
    fn visit_block(&mut self, block: &'a Block) {
        for stmt in &block.stmts {
            self.visit_item(stmt);
        }
    }
    
    fn visit_item(&mut self, item: &'a Item) {
        match *item {
            Item::Block(..) => self.visit_item_block(item),
            Item::Break(..) => self.visit_item_break(item),
            Item::Continue(..) => self.visit_item_continue(item),
            Item::Debugger => self.visit_item_debugger(item),
            Item::Do(..) => self.visit_item_do(item),
            Item::Empty => self.visit_item_empty(item),
            Item::ExprStmt(..) => self.visit_item_expr_stmt(item),
            Item::For(..) => self.visit_item_for(item),
            Item::ForIn(..) => self.visit_item_for_in(item),
            Item::ForVar(..) => self.visit_item_for_var(item),
            Item::ForVarIn(..) => self.visit_item_for_var_in(item),
            Item::Function(..) => self.visit_item_function(item),
            Item::If(..) => self.visit_item_if(item),
            Item::Leave(..) => self.visit_item_leave(item),
            Item::Return(..) => self.visit_item_return(item),
            Item::Switch(..) => self.visit_item_switch(item),
            Item::Throw(..) => self.visit_item_throw(item),
            Item::Try(..) => self.visit_item_try(item),
            Item::VarDecl(..) => self.visit_item_var_decl(item),
            Item::While(..) => self.visit_item_while(item),
            Item::With(..) => self.visit_item_with(item)
        }
    }
    
    fn visit_item_block(&mut self, item: &'a Item) {
        if let Item::Block(_, ref block) = *item {
            self.visit_block(block);
        }
    }
    
    #[allow(unused_variables)]
    fn visit_item_break(&mut self, item: &'a Item) {
        // Nothing to do.
    }
    
    #[allow(unused_variables)]
    fn visit_item_continue(&mut self, item: &'a Item) {
        // Nothing to do.
    }
    
    #[allow(unused_variables)]
    fn visit_item_debugger(&mut self, item: &'a Item) {
        // Nothing to do.
    }
    
    fn visit_item_do(&mut self, item: &'a Item) {
        if let Item::Do(_, ref expr, ref stmt) = *item {
            self.visit_expr(expr);
            self.visit_item(stmt);
        }
    }
    
    #[allow(unused_variables)]
    fn visit_item_empty(&mut self, item: &'a Item) {
        // Nothing to do.
    }
    
    fn visit_item_expr_stmt(&mut self, item: &'a Item) {
        if let Item::ExprStmt(ref exprs) = *item {
            self.visit_expr_seq(exprs);
        }
    }
    
    fn visit_item_for(&mut self, item: &'a Item) {
        if let Item::For(_, ref init, ref test, ref incr, ref stmt) = *item {
            if let Some(ref init) = *init {
                self.visit_expr_seq(init);
            }
            if let Some(ref test) = *test {
                self.visit_expr_seq(test);
            }
            if let Some(ref incr) = *incr {
                self.visit_expr_seq(incr);
            }
            self.visit_item(stmt);
        }
    }
    
    fn visit_item_for_in(&mut self, item: &'a Item) {
        if let Item::ForIn(_, ref expr, ref in_, ref stmt) = *item {
            self.visit_expr(expr);
            self.visit_expr_seq(in_);
            self.visit_item(stmt);
        }
    }
    
    fn visit_item_for_var(&mut self, item: &'a Item) {
        if let Item::ForVar(_, ref init, ref test, ref incr, ref stmt) = *item {
            if let Some(ref init) = *init {
                for var in init {
                    self.visit_var(var);
                }
            }
            if let Some(ref test) = *test {
                self.visit_expr_seq(test);
            }
            if let Some(ref incr) = *incr {
                self.visit_expr_seq(incr);
            }
            self.visit_item(stmt);
        }
    }
    
    fn visit_var(&mut self, var: &'a Var) {
        if let Some(ref expr) = var.expr {
            self.visit_expr(expr);
        }
    }
    
    fn visit_item_for_var_in(&mut self, item: &'a Item) {
        if let Item::ForVarIn(_, _, ref in_, ref stmt) = *item {
            self.visit_expr_seq(in_);
            self.visit_item(stmt);
        }
    }
    
    #[allow(unused_variables)]
    fn visit_item_function(&mut self, item: &'a Item) {
        // Nothing to do.
    }
    
    fn visit_item_if(&mut self, item: &'a Item) {
        if let Item::If(ref test, ref then, ref else_) = *item {
            self.visit_expr_seq(test);
            self.visit_item(then);
            if let Some(ref else_) = *else_ {
                self.visit_item(else_);
            }
        }
    }
    
    fn visit_item_leave(&mut self, item: &'a Item) {
        if let Item::Leave(ref item) = *item {
            self.visit_item(item);
        }
    }
    
    fn visit_item_return(&mut self, item: &'a Item) {
        if let Item::Return(ref exprs) = *item {
            if let Some(ref exprs) = *exprs {
                self.visit_expr_seq(exprs);
            }
        }
    }
    
    fn visit_item_switch(&mut self, item: &'a Item) {
        if let Item::Switch(_, ref exprs, ref clauses) = *item {
            self.visit_expr_seq(exprs);
            for clause in clauses {
                self.visit_switch_clause(clause);
            }
        }
    }
    
    fn visit_switch_clause(&mut self, switch_clause: &'a SwitchClause) {
        match *switch_clause {
            SwitchClause::Case(..) => self.visit_switch_clause_case(switch_clause),
            SwitchClause::Default(..) => self.visit_switch_clause_default(switch_clause)
        }
    }
    
    fn visit_switch_clause_case(&mut self, switch_clause: &'a SwitchClause) {
        if let SwitchClause::Case(ref exprs, ref items) = *switch_clause {
            self.visit_expr_seq(exprs);
            for item in items {
                self.visit_item(item);
            }
        }
    }
    
    fn visit_switch_clause_default(&mut self, switch_clause: &'a SwitchClause) {
        if let SwitchClause::Default(ref items) = *switch_clause {
            for item in items {
                self.visit_item(item);
            }
        }
    }
    
    fn visit_item_throw(&mut self, item: &'a Item) {
        if let Item::Throw(ref exprs) = *item {
            self.visit_expr_seq(exprs);
        }
    }
    
    fn visit_item_try(&mut self, item: &'a Item) {
        if let Item::Try(ref try, ref catch, ref finally) = *item {
            self.visit_block(try);
            if let Some(ref catch) = *catch {
                self.visit_catch(catch);
            }
            if let Some(ref finally) = *finally {
                self.visit_block(finally);
            }
        }
    }
    
    fn visit_catch(&mut self, catch: &'a Catch) {
        self.visit_block(&catch.block);
    }
    
    fn visit_item_var_decl(&mut self, item: &'a Item) {
        if let Item::VarDecl(ref vars) = *item {
            for var in vars {
                self.visit_var(var);
            }
        }
    }
    
    fn visit_item_while(&mut self, item: &'a Item) {
        if let Item::While(_, ref expr, ref stmt) = *item {
            self.visit_expr(expr);
            self.visit_item(stmt);
        }
    }
    
    fn visit_item_with(&mut self, item: &'a Item) {
        if let Item::With(ref exprs, ref stmt) = *item {
            self.visit_expr_seq(exprs);
            self.visit_item(stmt);
        }
    }
    
    fn visit_expr_seq(&mut self, exprs: &'a ExprSeq) {
        for expr in &exprs.exprs {
            self.visit_expr(expr);
        }
    }
    
    fn visit_expr(&mut self, expr: &'a Expr) {
        match *expr {
            Expr::ArrayLiteral(..) => self.visit_expr_array_literal(expr),
            Expr::Assign(..) => self.visit_expr_assign(expr),
            Expr::Binary(..) => self.visit_expr_binary(expr),
            Expr::Cast(..) => self.visit_expr_cast(expr),
            Expr::Call(..) => self.visit_expr_call(expr),
            Expr::Function(..) => self.visit_expr_function(expr),
            Expr::Ident(..) => self.visit_expr_ident(expr),
            Expr::Literal(..) => self.visit_expr_literal(expr),
            Expr::MemberDot(..) => self.visit_expr_member_dot(expr),
            Expr::MemberIndex(..) => self.visit_expr_member_index(expr),
            Expr::Missing(..) => self.visit_expr_missing(expr),
            Expr::New(..) => self.visit_expr_new(expr),
            Expr::ObjectLiteral(..) => self.visit_expr_object_literal(expr),
            Expr::Paren(..) => self.visit_expr_paren(expr),
            Expr::Ternary(..) => self.visit_expr_ternary(expr),
            Expr::This(..) => self.visit_expr_this(expr),
            Expr::Unary(..) => self.visit_expr_unary(expr)
        }
    }
    
    fn visit_expr_array_literal(&mut self, expr: &'a Expr) {
        if let Expr::ArrayLiteral(ref exprs) = *expr {
            for expr in exprs {
                self.visit_expr(expr);
            }
        }
    }
    
    fn visit_expr_assign(&mut self, expr: &'a Expr) {
        if let Expr::Assign(_, ref lhs, ref rhs) = *expr {
            self.visit_expr(lhs);
            self.visit_expr(rhs);
        }
    }
    
    fn visit_expr_binary(&mut self, expr: &'a Expr) {
        if let Expr::Binary(_, ref lhs, ref rhs) = *expr {
            self.visit_expr(lhs);
            self.visit_expr(rhs);
        }
    }
    
    fn visit_expr_cast(&mut self, expr: &'a Expr) {
        if let Expr::Cast(_, ref expr) = *expr {
            self.visit_expr(expr);
        }
    }
    
    fn visit_expr_call(&mut self, expr: &'a Expr) {
        if let Expr::Call(ref expr, ref args) = *expr {
            self.visit_expr(expr);
            for arg in args {
                self.visit_expr(arg);
            }
        }
    }
    
    #[allow(unused_variables)]
    fn visit_expr_function(&mut self, expr: &'a Expr) {
        // Nothing to do.
    }
    
    #[allow(unused_variables)]
    fn visit_expr_ident(&mut self, expr: &'a Expr) {
        // Nothing to do.
    }
    
    #[allow(unused_variables)]
    fn visit_expr_literal(&mut self, expr: &'a Expr) {
        // Nothing to do.
    }
    
    fn visit_expr_member_dot(&mut self, expr: &'a Expr) {
        if let Expr::MemberDot(ref expr, _) = *expr {
            self.visit_expr(expr);
        }
    }
    
    fn visit_expr_member_index(&mut self, expr: &'a Expr) {
        if let Expr::MemberIndex(ref expr, ref index) = *expr {
            self.visit_expr(expr);
            self.visit_expr_seq(index);
        }
    }
    
    #[allow(unused_variables)]
    fn visit_expr_missing(&mut self, expr: &'a Expr) {
        // Nothing to do.
    }
    
    fn visit_expr_new(&mut self, expr: &'a Expr) {
        if let Expr::New(ref expr) = *expr {
            self.visit_expr(expr);
        }
    }
    
    fn visit_expr_object_literal(&mut self, expr: &'a Expr) {
        if let Expr::ObjectLiteral(ref properties) = *expr {
            for property in properties {
                self.visit_property(property);
            }
        }
    }
    
    fn visit_property(&mut self, property: &'a Property) {
        match *property {
            Property::Assignment(..) => self.visit_property_assignment(property),
            Property::Getter(..) => self.visit_property_getter(property),
            Property::Setter(..) => self.visit_property_setter(property)
        }
    }
    
    fn visit_property_assignment(&mut self, property: &'a Property) {
        if let Property::Assignment(_, ref expr) = *property {
            self.visit_expr(expr);
        }
    }
    
    #[allow(unused)]
    fn visit_property_getter(&mut self, property: &'a Property) {
        // Nothing to do.
    }
    
    #[allow(unused)]
    fn visit_property_setter(&mut self, property: &'a Property) {
        // Nothing to do.
    }
    
    fn visit_expr_paren(&mut self, expr: &'a Expr) {
        if let Expr::Paren(ref exprs) = *expr {
            self.visit_expr_seq(exprs);
        }
    }
    
    fn visit_expr_ternary(&mut self, expr: &'a Expr) {
        if let Expr::Ternary(ref test, ref then, ref else_) = *expr {
            self.visit_expr(test);
            self.visit_expr(then);
            self.visit_expr(else_);
        }
    }
    
    #[allow(unused_variables)]
    fn visit_expr_this(&mut self, expr: &'a Expr) {
        // Nothing to do.
    }
    
    fn visit_expr_unary(&mut self, expr: &'a Expr) {
        if let Expr::Unary(_, ref expr) = *expr {
            self.visit_expr(expr);
        }
    }
}
