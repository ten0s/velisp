const {Bool, Sym, Str, List, Fun, ensureType} = require('../VeLispTypes.js');
const VeDclDialogsLoader = require('../VeDclDialogsLoader.js');
const {VeDclGtkDialog} = require('../VeDclGtkDialog.js');

const gi = require('node-gtk');
const Gtk = gi.require('Gtk', '3.0');

gi.startLoop();

exports.initContext = function (context) {
    context.setSym('LOAD_DIALOG', new Fun('load_dialog', ['dclfile'], [], (self, args) => {
        if (args.length < 1) {
            throw new Error('load_dialog: too few arguments');
        }
        if (args.length > 1) {
            throw new Error('load_dialog: too many arguments');
        }
        const dclfile = ensureType('load_dialog:', args[0], [Str]);
        // TODO: store to context.setDialogs(...)?
        const dialogs = VeDclDialogsLoader.load(dclfile.value());
        // TODO: should be called from (new_dialog ...) and (start_dialog)
        const gtkDialog = new VeDclGtkDialog(null);
        return new Str(gtkDialog);
    }));
    context.setSym('START_DIALOG', new Fun('start_dialog', [], [], (self, args) => {
        if (args.length > 0) {
            throw new Error('start_dialog: too many arguments');
        }

        Gtk.init();

        const win = new Gtk.Window();
        win.on('destroy', () => Gtk.mainQuit());
        win.on('delete-event', () => false);

        win.setDefaultSize(200, 80);
        win.add(new Gtk.Label({ label: 'Hello Gtk+' }));

        win.showAll();
        Gtk.main();
    }));
}
