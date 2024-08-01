use serde::{Deserialize, Serialize};
use serde_json as json;

// or notification
#[derive(Serialize, Deserialize)]
pub struct LspRequest {
    pub jsonrpc: String,
    pub id: Option<i32>,
    pub method: String,
    pub params: json::Value,
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
    pub result: json::Value,
    pub error: Option<LspResponseError>,
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn test_lsp_request() {
        let json_str = r#"{
            "jsonrpc": "2.0",
            "id": 1,
            "method": "initialize",
            "params": {
                "xxx": 123
            }
        }"#;
        let req: LspRequest = json::from_str(json_str).unwrap();
        assert_eq!(req.jsonrpc, "2.0");
        assert_eq!(req.id, Some(1));
        assert_eq!(req.method, "initialize");
        assert!(!req.is_notification());
    }

    #[test]
    fn test_lsp_request_notification() {
        let json_str = r#"{
            "jsonrpc": "2.0",
            "method": "initialized",
            "params": {}
        }"#;
        let req: LspRequest = json::from_str(json_str).unwrap();
        assert_eq!(req.id, None);
        assert_eq!(req.method, "initialized");
        assert!(req.is_notification());
    }

    #[test]
    fn test_lsp_response_serialization() {
        let resp = LspResponse {
            jsonrpc: "2.0".to_string(),
            id: 1,
            result: json::Value::Null,
            error: Some(LspResponseError {
                code: 123,
                message: "asdf".to_string(),
            }),
        };
        let resp_str = json::to_string(&resp).unwrap();
        assert_eq!(
            resp_str,
            r#"{"jsonrpc":"2.0","id":1,"result":null,"error":{"code":123,"message":"asdf"}}"#
        );
    }
}
