use rt::{JsItem, JsEnv, JsValue, JsHandle};

pub struct JsNumber {
    value: f64
}

impl JsNumber {
    pub fn new(value: f64) -> JsNumber {
        JsNumber {
            value: value
        }
    }
}

impl JsItem for JsNumber {
    fn as_value(&self) -> JsValue {
        JsValue::new_number(self.value)
    }
    
    fn has_prototype(&self) -> bool {
        true
    }
    
    fn prototype(&self, env: &JsEnv) -> Option<JsValue> {
        Some(env.handle(JsHandle::Number).as_value())
    }
}
