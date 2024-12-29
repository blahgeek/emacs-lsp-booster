use serde::{ Deserialize, Serialize };

// or notification
#[derive(Serialize, Deserialize)]
pub struct LspRequest {
    pub jsonrpc: String,
    pub id: Option<i32>,
    pub method: String,
    pub params: serde_json::Value,
}

impl LspRequest {
    pub fn is_notification(&self) -> bool {
        self.id.is_none()
    }
}

#[derive(Serialize, Deserialize)]
pub struct LspResponseError {
    pub code: i32,
    pub message: String,
}

#[derive(Serialize, Deserialize)]
pub struct LspResponse {
    pub jsonrpc: String,
    pub id: i32,
    pub result: serde_json::Value,
    pub error: Option<LspResponseError>,
}

#[cfg(test)]
mod test {
    use super::*;
    use simd_json as json;

    #[test]
    fn test_lsp_request() {
        let mut json_str = r#"{
            "jsonrpc": "2.0",
            "id": 1,
            "method": "initialize",
            "params": {
                "xxx": 123
            }
        }"#.to_string();
        let req: LspRequest = unsafe {json::from_str(&mut json_str).unwrap()};
        assert_eq!(req.jsonrpc, "2.0");
        assert_eq!(req.id, Some(1));
        assert_eq!(req.method, "initialize");
        assert_eq!(req.is_notification(), false);
    }

    #[test]
    fn test_lsp_request_notification() {
        let mut json_str = r#"{
            "jsonrpc": "2.0",
            "method": "initialized",
            "params": {}
        }"#.to_string();
        let req: LspRequest = unsafe {
        json::from_str(&mut json_str).unwrap()
        };
        assert_eq!(req.id, None);
        assert_eq!(req.method, "initialized");
        assert_eq!(req.is_notification(), true);
    }

    #[test]
    fn test_lsp_response_serialization() {
        let resp = LspResponse {
            jsonrpc: "2.0".to_string(),
            id: 1,
            result: serde_json::Value::Null,
            error: Some(LspResponseError {
                code: 123,
                message: "asdf".to_string(),
            }),
        };
        let resp_str = json::to_string(&resp).unwrap();
        assert_eq!(resp_str, r#"{"jsonrpc":"2.0","id":1,"result":null,"error":{"code":123,"message":"asdf"}}"#);
    }
}
