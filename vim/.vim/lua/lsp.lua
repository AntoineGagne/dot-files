local module = {};

module.custom_settings = {
    bashls = {};
    elmls = {};
    hie = {};
    purescriptls = {
       log_level = vim.lsp.protocol.MessageType.Log;
       message_level = vim.lsp.protocol.MessageType.Log;
    };
    texlab = {};
};

return module;
