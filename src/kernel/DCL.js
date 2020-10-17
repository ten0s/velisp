const {Bool, Sym, List, Fun} = require('../VeLispTypes.js');

const gi = require('node-gtk');
const Gtk = gi.require('Gtk', '3.0');

gi.startLoop();
Gtk.init();

exports.initContext = function (context) {
    context.setSym('START_DIALOG', new Fun('start_dialog', [], [], (self, args) => {
        if (args.length > 0) {
            throw new Error('start_dialog: too many arguments');
        }

        const win = new Gtk.Window();
        win.on('destroy', () => Gtk.mainQuit());
        win.on('delete-event', () => false);

        win.setDefaultSize(200, 80);
        win.add(new Gtk.Label({ label: 'Hello Gtk+' }));

        win.showAll();
        Gtk.main();
    }));
}
