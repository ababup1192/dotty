let editor;
const markers = [];
const fontSize = 10;

const initialize = (app) => {
    require('brace/mode/javascript');
    require('brace/theme/chrome');

    editor = ace.edit("textEditor");
    editor.$blockScrolling = Infinity;
    editor.setTheme("ace/theme/chrome");
    editor.setFontSize(fontSize);
    editor.getSession().setMode("ace/mode/javascript");
    editor.setOption("dragEnabled", true); // true by default anyway
    editor.setOption("highlightActiveLine", false);
    editor.setShowPrintMargin(false);
    editor.getSession().setUseWrapMode(true);
    editor.getSession().setUseSoftTabs(true);
    editor.getSession().setTabSize(2);

    editor.on("input", () => {
        const info = editor.getSession().getDocument().getValue();
        app.ports.receiveEditorState.send({ code: info });
    });
};

const displayCode = (code) =>
    editor.getSession().setValue(code, 0)

export const subscribe = (app) => {
    app.ports.aceCodeBoxCmd.subscribe((aceCmd) => {
        const message = aceCmd.message;
        const code = aceCmd.code;

        if (message === "initializeAndDisplay") {
            initialize(app);
            displayCode(code);
        } else if (message === "displayCode") {
            displayCode(code)
        }
    });
};