let editor;
const markers = [];
const fontSize = 10;

function initialize(app) {
    require('brace/mode/javascript');
    require('brace/theme/chrome');

    editor = ace.edit("editor");
    editor.$blockScrolling = Infinity;
    editor.setTheme("ace/theme/chrome");
    editor.setFontSize(fontSize);
    editor.getSession().setMode("ace/mode/javascript");
    editor.setOption("dragEnabled", true); // true by default anyway
    editor.setOption("highlightActiveLine", false);
    editor.setShowPrintMargin(false);
    editor.getSession().setUseSoftTabs(true);
    editor.getSession().setTabSize(2);

    
    editor.on("input", () => {
        const info = editor.getSession().getDocument().getValue();
        app.ports.receiveEditorState.send({code: info});
    });

    /*
    editor.selection.on("changeCursor", () => {
        const info = getEditorState();
        app.ports.receiveEditorState.send(info);
    });

    editor.getSession().on("changeScrollTop", () => {
        const info = getEditorState();
        app.ports.receiveEditorState.send(info);

    });
    editor.getSession().on("changeScrollLeft", () => {
        const info = getEditorState();
        app.ports.receiveEditorState.send(info);
    });
    */
}

export const subscribe = (app) => {
    app.ports.aceCodeBoxCmd.subscribe((aceCmd) => {
        var message = aceCmd.message;

        if (message === "initializeAndDisplay") {
            initialize(app);
        }
    });
};