use rt::{JsItem, JsEnv, JsValue, JsHandle};

pub struct JsBoolean {
    value: bool
}

impl JsBoolean {
    pub fn new(value: bool) -> JsBoolean {
        JsBoolean {
            value: value
        }
    }
}

impl JsItem for JsBoolean {
    fn as_value(&self) -> JsValue {
        JsValue::new_bool(self.value)
    }
    
    fn has_prototype(&self) -> bool {
        true
    }
    
    fn prototype(&self, env: &JsEnv) -> Option<JsValue> {
        Some(env.handle(JsHandle::Boolean).as_value())
    }
}
