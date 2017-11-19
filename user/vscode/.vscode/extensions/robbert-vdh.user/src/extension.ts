import * as copy from 'copy-paste';
import * as vscode from 'vscode';

export function activate(context: vscode.ExtensionContext) {
    let terminal: vscode.Terminal = null;

    // Allows the user to evaluate the selected text in IPython. NumPy and
    // matploblib get imported automatically.
    context.subscriptions.push(vscode.commands.registerCommand('user.ipython.send', () => {
        if (terminal == null) {
            terminal = vscode.window.createTerminal('IPython', '/usr/bin/ipython', ['--pylab']);
        }

        // Either send the selected text or the entire file
        let text = "";
        const document = vscode.window.activeTextEditor.document;
        const selection = vscode.window.activeTextEditor.selection;
        if (selection.isEmpty) {
            text = document.getText();
        } else {
            text = document.getText(selection);
        }

        copy.copy(text, () => {
            terminal.sendText("%paste -q");
            terminal.show(true);
        })
    }));
}
