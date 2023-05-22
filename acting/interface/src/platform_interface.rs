#[allow(clippy::derive_partial_eq_without_eq)]
#[derive(Clone, PartialEq, ::prost::Message)]
pub struct StateVariable {
    #[prost(enumeration = "StateVariableType", tag = "1")]
    pub r#type: i32,
    #[prost(string, tag = "2")]
    pub state_function: ::prost::alloc::string::String,
    #[prost(message, repeated, tag = "3")]
    pub parameters: ::prost::alloc::vec::Vec<Atom>,
    #[prost(message, optional, tag = "4")]
    pub value: ::core::option::Option<Expression>,
}
#[allow(clippy::derive_partial_eq_without_eq)]
#[derive(Clone, PartialEq, ::prost::Message)]
pub struct StateUpdate {
    #[prost(message, repeated, tag = "1")]
    pub state_variables: ::prost::alloc::vec::Vec<StateVariable>,
}
#[allow(clippy::derive_partial_eq_without_eq)]
#[derive(Clone, PartialEq, ::prost::Message)]
pub struct Atom {
    #[prost(oneof = "atom::Kind", tags = "1, 2, 3, 4")]
    pub kind: ::core::option::Option<atom::Kind>,
}
/// Nested message and enum types in `Atom`.
pub mod atom {
    #[allow(clippy::derive_partial_eq_without_eq)]
    #[derive(Clone, PartialEq, ::prost::Oneof)]
    pub enum Kind {
        #[prost(string, tag = "1")]
        Symbol(::prost::alloc::string::String),
        #[prost(int64, tag = "2")]
        Int(i64),
        #[prost(double, tag = "3")]
        Float(f64),
        #[prost(bool, tag = "4")]
        Boolean(bool),
    }
}
#[allow(clippy::derive_partial_eq_without_eq)]
#[derive(Clone, PartialEq, ::prost::Message)]
pub struct Expression {
    /// For instance `3`, `+`, `kitchen`, `at-robot`, ...
    #[prost(message, optional, tag = "1")]
    pub atom: ::core::option::Option<Atom>,
    /// If the `atom` field is empty, then the expression is a list of sub-expressions,
    /// typically representing the application of some arguments to a function or fluent.
    /// For instance `(+ 1 3)`, (at-robot l1)`, `(>= (battery_level) 20)`
    #[prost(message, repeated, tag = "2")]
    pub list: ::prost::alloc::vec::Vec<Expression>,
}
#[allow(clippy::derive_partial_eq_without_eq)]
#[derive(Clone, PartialEq, ::prost::Message)]
pub struct CommandRequest {
    #[prost(oneof = "command_request::Request", tags = "1, 2")]
    pub request: ::core::option::Option<command_request::Request>,
}
/// Nested message and enum types in `CommandRequest`.
pub mod command_request {
    #[allow(clippy::derive_partial_eq_without_eq)]
    #[derive(Clone, PartialEq, ::prost::Oneof)]
    pub enum Request {
        #[prost(message, tag = "1")]
        Execution(super::CommandExecutionRequest),
        #[prost(message, tag = "2")]
        Cancel(super::CommandCancelRequest),
    }
}
#[allow(clippy::derive_partial_eq_without_eq)]
#[derive(Clone, PartialEq, ::prost::Message)]
pub struct CommandExecutionRequest {
    #[prost(message, repeated, tag = "1")]
    pub arguments: ::prost::alloc::vec::Vec<Expression>,
    #[prost(uint64, tag = "2")]
    pub command_id: u64,
}
#[allow(clippy::derive_partial_eq_without_eq)]
#[derive(Clone, PartialEq, ::prost::Message)]
pub struct CommandCancelRequest {
    #[prost(uint64, tag = "1")]
    pub command_id: u64,
}
#[allow(clippy::derive_partial_eq_without_eq)]
#[derive(Clone, PartialEq, ::prost::Message)]
pub struct CommandResponse {
    #[prost(oneof = "command_response::Response", tags = "1, 2, 3, 4, 5")]
    pub response: ::core::option::Option<command_response::Response>,
}
/// Nested message and enum types in `CommandResponse`.
pub mod command_response {
    #[allow(clippy::derive_partial_eq_without_eq)]
    #[derive(Clone, PartialEq, ::prost::Oneof)]
    pub enum Response {
        #[prost(message, tag = "1")]
        Accepted(super::CommandAccepted),
        #[prost(message, tag = "2")]
        Rejected(super::CommandRejected),
        #[prost(message, tag = "3")]
        Progress(super::CommandProgress),
        #[prost(message, tag = "4")]
        Result(super::CommandResult),
        #[prost(message, tag = "5")]
        Cancelled(super::CommandCancelled),
    }
}
#[allow(clippy::derive_partial_eq_without_eq)]
#[derive(Clone, PartialEq, ::prost::Message)]
pub struct CommandRejected {
    #[prost(uint64, tag = "1")]
    pub command_id: u64,
}
#[allow(clippy::derive_partial_eq_without_eq)]
#[derive(Clone, PartialEq, ::prost::Message)]
pub struct CommandAccepted {
    #[prost(uint64, tag = "1")]
    pub command_id: u64,
}
#[allow(clippy::derive_partial_eq_without_eq)]
#[derive(Clone, PartialEq, ::prost::Message)]
pub struct CommandProgress {
    #[prost(uint64, tag = "1")]
    pub command_id: u64,
    #[prost(double, tag = "2")]
    pub progress: f64,
}
#[allow(clippy::derive_partial_eq_without_eq)]
#[derive(Clone, PartialEq, ::prost::Message)]
pub struct CommandResult {
    #[prost(uint64, tag = "1")]
    pub command_id: u64,
    #[prost(bool, tag = "2")]
    pub result: bool,
}
#[allow(clippy::derive_partial_eq_without_eq)]
#[derive(Clone, PartialEq, ::prost::Message)]
pub struct CommandCancelled {
    #[prost(uint64, tag = "1")]
    pub command_id: u64,
    #[prost(bool, tag = "2")]
    pub result: bool,
}
#[allow(clippy::derive_partial_eq_without_eq)]
#[derive(Clone, PartialEq, ::prost::Message)]
pub struct InitGetUpdate {}
#[allow(clippy::derive_partial_eq_without_eq)]
#[derive(Clone, PartialEq, ::prost::Message)]
pub struct Event {
    #[prost(oneof = "event::Event", tags = "1, 2")]
    pub event: ::core::option::Option<event::Event>,
}
/// Nested message and enum types in `Event`.
pub mod event {
    #[allow(clippy::derive_partial_eq_without_eq)]
    #[derive(Clone, PartialEq, ::prost::Oneof)]
    pub enum Event {
        #[prost(message, tag = "1")]
        Instance(super::Instance),
        #[prost(message, tag = "2")]
        Resource(super::Resource),
    }
}
#[allow(clippy::derive_partial_eq_without_eq)]
#[derive(Clone, PartialEq, ::prost::Message)]
pub struct Instance {
    #[prost(string, tag = "1")]
    pub r#type: ::prost::alloc::string::String,
    #[prost(string, tag = "2")]
    pub object: ::prost::alloc::string::String,
}
#[allow(clippy::derive_partial_eq_without_eq)]
#[derive(Clone, PartialEq, ::prost::Message)]
pub struct Resource {
    #[prost(string, tag = "1")]
    pub label: ::prost::alloc::string::String,
    #[prost(enumeration = "ResourceKind", tag = "2")]
    pub resource_kind: i32,
    #[prost(uint64, tag = "3")]
    pub quantity: u64,
}
#[allow(clippy::derive_partial_eq_without_eq)]
#[derive(Clone, PartialEq, ::prost::Message)]
pub struct PlatformUpdate {
    #[prost(oneof = "platform_update::Update", tags = "1, 2")]
    pub update: ::core::option::Option<platform_update::Update>,
}
/// Nested message and enum types in `PlatformUpdate`.
pub mod platform_update {
    #[allow(clippy::derive_partial_eq_without_eq)]
    #[derive(Clone, PartialEq, ::prost::Oneof)]
    pub enum Update {
        #[prost(message, tag = "1")]
        State(super::StateUpdate),
        #[prost(message, tag = "2")]
        Event(super::Event),
    }
}
#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash, PartialOrd, Ord, ::prost::Enumeration)]
#[repr(i32)]
pub enum StateVariableType {
    Static = 0,
    Dynamic = 1,
}
impl StateVariableType {
    /// String value of the enum field names used in the ProtoBuf definition.
    ///
    /// The values are not transformed in any way and thus are considered stable
    /// (if the ProtoBuf definition does not change) and safe for programmatic use.
    pub fn as_str_name(&self) -> &'static str {
        match self {
            StateVariableType::Static => "STATIC",
            StateVariableType::Dynamic => "DYNAMIC",
        }
    }
    /// Creates an enum from field names used in the ProtoBuf definition.
    pub fn from_str_name(value: &str) -> ::core::option::Option<Self> {
        match value {
            "STATIC" => Some(Self::Static),
            "DYNAMIC" => Some(Self::Dynamic),
            _ => None,
        }
    }
}
#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash, PartialOrd, Ord, ::prost::Enumeration)]
#[repr(i32)]
pub enum ResourceKind {
    Unary = 0,
    Divisible = 1,
}
impl ResourceKind {
    /// String value of the enum field names used in the ProtoBuf definition.
    ///
    /// The values are not transformed in any way and thus are considered stable
    /// (if the ProtoBuf definition does not change) and safe for programmatic use.
    pub fn as_str_name(&self) -> &'static str {
        match self {
            ResourceKind::Unary => "Unary",
            ResourceKind::Divisible => "Divisible",
        }
    }
    /// Creates an enum from field names used in the ProtoBuf definition.
    pub fn from_str_name(value: &str) -> ::core::option::Option<Self> {
        match value {
            "Unary" => Some(Self::Unary),
            "Divisible" => Some(Self::Divisible),
            _ => None,
        }
    }
}
/// Generated client implementations.
pub mod platform_interface_client {
    #![allow(unused_variables, dead_code, missing_docs, clippy::let_unit_value)]
    use tonic::codegen::*;
    use tonic::codegen::http::Uri;
    #[derive(Debug, Clone)]
    pub struct PlatformInterfaceClient<T> {
        inner: tonic::client::Grpc<T>,
    }
    impl PlatformInterfaceClient<tonic::transport::Channel> {
        /// Attempt to create a new client by connecting to a given endpoint.
        pub async fn connect<D>(dst: D) -> Result<Self, tonic::transport::Error>
        where
            D: std::convert::TryInto<tonic::transport::Endpoint>,
            D::Error: Into<StdError>,
        {
            let conn = tonic::transport::Endpoint::new(dst)?.connect().await?;
            Ok(Self::new(conn))
        }
    }
    impl<T> PlatformInterfaceClient<T>
    where
        T: tonic::client::GrpcService<tonic::body::BoxBody>,
        T::Error: Into<StdError>,
        T::ResponseBody: Body<Data = Bytes> + Send + 'static,
        <T::ResponseBody as Body>::Error: Into<StdError> + Send,
    {
        pub fn new(inner: T) -> Self {
            let inner = tonic::client::Grpc::new(inner);
            Self { inner }
        }
        pub fn with_origin(inner: T, origin: Uri) -> Self {
            let inner = tonic::client::Grpc::with_origin(inner, origin);
            Self { inner }
        }
        pub fn with_interceptor<F>(
            inner: T,
            interceptor: F,
        ) -> PlatformInterfaceClient<InterceptedService<T, F>>
        where
            F: tonic::service::Interceptor,
            T::ResponseBody: Default,
            T: tonic::codegen::Service<
                http::Request<tonic::body::BoxBody>,
                Response = http::Response<
                    <T as tonic::client::GrpcService<tonic::body::BoxBody>>::ResponseBody,
                >,
            >,
            <T as tonic::codegen::Service<
                http::Request<tonic::body::BoxBody>,
            >>::Error: Into<StdError> + Send + Sync,
        {
            PlatformInterfaceClient::new(InterceptedService::new(inner, interceptor))
        }
        /// Compress requests with the given encoding.
        ///
        /// This requires the server to support it otherwise it might respond with an
        /// error.
        #[must_use]
        pub fn send_compressed(mut self, encoding: CompressionEncoding) -> Self {
            self.inner = self.inner.send_compressed(encoding);
            self
        }
        /// Enable decompressing responses.
        #[must_use]
        pub fn accept_compressed(mut self, encoding: CompressionEncoding) -> Self {
            self.inner = self.inner.accept_compressed(encoding);
            self
        }
        pub async fn get_updates(
            &mut self,
            request: impl tonic::IntoRequest<super::InitGetUpdate>,
        ) -> Result<
            tonic::Response<tonic::codec::Streaming<super::PlatformUpdate>>,
            tonic::Status,
        > {
            self.inner
                .ready()
                .await
                .map_err(|e| {
                    tonic::Status::new(
                        tonic::Code::Unknown,
                        format!("Service was not ready: {}", e.into()),
                    )
                })?;
            let codec = tonic::codec::ProstCodec::default();
            let path = http::uri::PathAndQuery::from_static(
                "/platform_interface/GetUpdates",
            );
            self.inner.server_streaming(request.into_request(), path, codec).await
        }
        pub async fn send_commands(
            &mut self,
            request: impl tonic::IntoStreamingRequest<Message = super::CommandRequest>,
        ) -> Result<
            tonic::Response<tonic::codec::Streaming<super::CommandResponse>>,
            tonic::Status,
        > {
            self.inner
                .ready()
                .await
                .map_err(|e| {
                    tonic::Status::new(
                        tonic::Code::Unknown,
                        format!("Service was not ready: {}", e.into()),
                    )
                })?;
            let codec = tonic::codec::ProstCodec::default();
            let path = http::uri::PathAndQuery::from_static(
                "/platform_interface/SendCommands",
            );
            self.inner.streaming(request.into_streaming_request(), path, codec).await
        }
    }
}
/// Generated server implementations.
pub mod platform_interface_server {
    #![allow(unused_variables, dead_code, missing_docs, clippy::let_unit_value)]
    use tonic::codegen::*;
    /// Generated trait containing gRPC methods that should be implemented for use with PlatformInterfaceServer.
    #[async_trait]
    pub trait PlatformInterface: Send + Sync + 'static {
        /// Server streaming response type for the GetUpdates method.
        type GetUpdatesStream: futures_core::Stream<
                Item = Result<super::PlatformUpdate, tonic::Status>,
            >
            + Send
            + 'static;
        async fn get_updates(
            &self,
            request: tonic::Request<super::InitGetUpdate>,
        ) -> Result<tonic::Response<Self::GetUpdatesStream>, tonic::Status>;
        /// Server streaming response type for the SendCommands method.
        type SendCommandsStream: futures_core::Stream<
                Item = Result<super::CommandResponse, tonic::Status>,
            >
            + Send
            + 'static;
        async fn send_commands(
            &self,
            request: tonic::Request<tonic::Streaming<super::CommandRequest>>,
        ) -> Result<tonic::Response<Self::SendCommandsStream>, tonic::Status>;
    }
    #[derive(Debug)]
    pub struct PlatformInterfaceServer<T: PlatformInterface> {
        inner: _Inner<T>,
        accept_compression_encodings: EnabledCompressionEncodings,
        send_compression_encodings: EnabledCompressionEncodings,
    }
    struct _Inner<T>(Arc<T>);
    impl<T: PlatformInterface> PlatformInterfaceServer<T> {
        pub fn new(inner: T) -> Self {
            Self::from_arc(Arc::new(inner))
        }
        pub fn from_arc(inner: Arc<T>) -> Self {
            let inner = _Inner(inner);
            Self {
                inner,
                accept_compression_encodings: Default::default(),
                send_compression_encodings: Default::default(),
            }
        }
        pub fn with_interceptor<F>(
            inner: T,
            interceptor: F,
        ) -> InterceptedService<Self, F>
        where
            F: tonic::service::Interceptor,
        {
            InterceptedService::new(Self::new(inner), interceptor)
        }
        /// Enable decompressing requests with the given encoding.
        #[must_use]
        pub fn accept_compressed(mut self, encoding: CompressionEncoding) -> Self {
            self.accept_compression_encodings.enable(encoding);
            self
        }
        /// Compress responses with the given encoding, if the client supports it.
        #[must_use]
        pub fn send_compressed(mut self, encoding: CompressionEncoding) -> Self {
            self.send_compression_encodings.enable(encoding);
            self
        }
    }
    impl<T, B> tonic::codegen::Service<http::Request<B>> for PlatformInterfaceServer<T>
    where
        T: PlatformInterface,
        B: Body + Send + 'static,
        B::Error: Into<StdError> + Send + 'static,
    {
        type Response = http::Response<tonic::body::BoxBody>;
        type Error = std::convert::Infallible;
        type Future = BoxFuture<Self::Response, Self::Error>;
        fn poll_ready(
            &mut self,
            _cx: &mut Context<'_>,
        ) -> Poll<Result<(), Self::Error>> {
            Poll::Ready(Ok(()))
        }
        fn call(&mut self, req: http::Request<B>) -> Self::Future {
            let inner = self.inner.clone();
            match req.uri().path() {
                "/platform_interface/GetUpdates" => {
                    #[allow(non_camel_case_types)]
                    struct GetUpdatesSvc<T: PlatformInterface>(pub Arc<T>);
                    impl<
                        T: PlatformInterface,
                    > tonic::server::ServerStreamingService<super::InitGetUpdate>
                    for GetUpdatesSvc<T> {
                        type Response = super::PlatformUpdate;
                        type ResponseStream = T::GetUpdatesStream;
                        type Future = BoxFuture<
                            tonic::Response<Self::ResponseStream>,
                            tonic::Status,
                        >;
                        fn call(
                            &mut self,
                            request: tonic::Request<super::InitGetUpdate>,
                        ) -> Self::Future {
                            let inner = self.0.clone();
                            let fut = async move { (*inner).get_updates(request).await };
                            Box::pin(fut)
                        }
                    }
                    let accept_compression_encodings = self.accept_compression_encodings;
                    let send_compression_encodings = self.send_compression_encodings;
                    let inner = self.inner.clone();
                    let fut = async move {
                        let inner = inner.0;
                        let method = GetUpdatesSvc(inner);
                        let codec = tonic::codec::ProstCodec::default();
                        let mut grpc = tonic::server::Grpc::new(codec)
                            .apply_compression_config(
                                accept_compression_encodings,
                                send_compression_encodings,
                            );
                        let res = grpc.server_streaming(method, req).await;
                        Ok(res)
                    };
                    Box::pin(fut)
                }
                "/platform_interface/SendCommands" => {
                    #[allow(non_camel_case_types)]
                    struct SendCommandsSvc<T: PlatformInterface>(pub Arc<T>);
                    impl<
                        T: PlatformInterface,
                    > tonic::server::StreamingService<super::CommandRequest>
                    for SendCommandsSvc<T> {
                        type Response = super::CommandResponse;
                        type ResponseStream = T::SendCommandsStream;
                        type Future = BoxFuture<
                            tonic::Response<Self::ResponseStream>,
                            tonic::Status,
                        >;
                        fn call(
                            &mut self,
                            request: tonic::Request<
                                tonic::Streaming<super::CommandRequest>,
                            >,
                        ) -> Self::Future {
                            let inner = self.0.clone();
                            let fut = async move {
                                (*inner).send_commands(request).await
                            };
                            Box::pin(fut)
                        }
                    }
                    let accept_compression_encodings = self.accept_compression_encodings;
                    let send_compression_encodings = self.send_compression_encodings;
                    let inner = self.inner.clone();
                    let fut = async move {
                        let inner = inner.0;
                        let method = SendCommandsSvc(inner);
                        let codec = tonic::codec::ProstCodec::default();
                        let mut grpc = tonic::server::Grpc::new(codec)
                            .apply_compression_config(
                                accept_compression_encodings,
                                send_compression_encodings,
                            );
                        let res = grpc.streaming(method, req).await;
                        Ok(res)
                    };
                    Box::pin(fut)
                }
                _ => {
                    Box::pin(async move {
                        Ok(
                            http::Response::builder()
                                .status(200)
                                .header("grpc-status", "12")
                                .header("content-type", "application/grpc")
                                .body(empty_body())
                                .unwrap(),
                        )
                    })
                }
            }
        }
    }
    impl<T: PlatformInterface> Clone for PlatformInterfaceServer<T> {
        fn clone(&self) -> Self {
            let inner = self.inner.clone();
            Self {
                inner,
                accept_compression_encodings: self.accept_compression_encodings,
                send_compression_encodings: self.send_compression_encodings,
            }
        }
    }
    impl<T: PlatformInterface> Clone for _Inner<T> {
        fn clone(&self) -> Self {
            Self(self.0.clone())
        }
    }
    impl<T: std::fmt::Debug> std::fmt::Debug for _Inner<T> {
        fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
            write!(f, "{:?}", self.0)
        }
    }
    impl<T: PlatformInterface> tonic::server::NamedService
    for PlatformInterfaceServer<T> {
        const NAME: &'static str = "platform_interface";
    }
}
