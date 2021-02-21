local module = {};

module.custom_settings = {
    bashls = {};
    cssls = {};
    elmls = {};
    erlangls = {};
    hie = {};
    hls = {};
    purescriptls = {
       log_level = vim.lsp.protocol.MessageType.Log;
       message_level = vim.lsp.protocol.MessageType.Log;
    };
    rust_analyzer = {};
    texlab = {};
    tsserver = {};
};

return module;
