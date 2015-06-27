use rt::{JsItem, JsEnv, JsValue, JsHandle};
use gc::Local;

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
    fn as_value(&self, env: &JsEnv) -> Local<JsValue> {
        env.new_bool(self.value)
    }
    
    fn has_prototype(&self, _: &JsEnv) -> bool {
        true
    }
    
    fn prototype(&self, env: &JsEnv) -> Option<Local<JsValue>> {
        Some(env.handle(JsHandle::Boolean).as_value(env))
    }
}
