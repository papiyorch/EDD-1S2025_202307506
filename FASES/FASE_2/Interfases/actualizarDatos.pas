unit actualizarDatos;

{$MODE DELPHI}

interface
    procedure showActualizarDatosWindow;

implementation
    uses
        SysUtils, Classes, InterfaceTools, ListaSimple, gtk2, glib2, gdk2, variables;

    var
        actualizarDatosWindow: PGtkWidget;
        entryUserUpdate, entryPhoneUpdate: PGtkWidget;

    procedure updateUserData(widget: PGtkWidget; data: gpointer); cdecl;
    var
        newUsername, newPhone: String;
    begin
        newUsername := Trim(gtk_entry_get_text(GTK_ENTRY(entryUserUpdate)));
        newPhone := Trim(gtk_entry_get_text(GTK_ENTRY(entryPhoneUpdate)));

        if usuarioActual <> nil then
        begin
            usuarioActual^.usuario := newUsername;
            usuarioActual^.telefono := newPhone;
            mostrarMensajeLogin(actualizarDatosWindow, 'Datos Actualizados', 'Los datos han sido actualizados correctamente.');
        end
        else
        begin
            mostrarMensajeLogin(actualizarDatosWindow, 'Error', 'No hay usuario seleccionado para actualizar.');
        end;
    end;

    procedure showActualizarDatosWindow;
    var
        grid: PGtkWidget;
        btnGuardarCambios: PGtkWidget;

    begin
        gtk_init(@argc, @argv);

        actualizarDatosWindow := gtk_window_new(GTK_WINDOW_TOPLEVEL);
        gtk_window_set_title(GTK_WINDOW(actualizarDatosWindow), 'Actualizar Datos');
        gtk_container_set_border_width(GTK_CONTAINER(actualizarDatosWindow), 10);
        gtk_window_set_default_size(GTK_WINDOW(actualizarDatosWindow), 400, 200);

        grid := gtk_table_new(3, 2, False);
        gtk_container_add(GTK_CONTAINER(actualizarDatosWindow), grid);

        entryUserUpdate := gtk_entry_new();
        entryPhoneUpdate := gtk_entry_new();

        gtk_table_attach_defaults(GTK_TABLE(grid), gtk_label_new('Usuario:'), 0, 1, 0, 1);
        gtk_table_attach_defaults(GTK_TABLE(grid), entryUserUpdate, 1, 2, 0, 1);
        gtk_table_attach_defaults(GTK_TABLE(grid), gtk_label_new('Teléfono:'), 0, 1, 1, 2);
        gtk_table_attach_defaults(GTK_TABLE(grid), entryPhoneUpdate, 1, 2, 1, 2);

        btnGuardarCambios := gtk_button_new_with_label('Actualizar');
        g_signal_connect(btnGuardarCambios, 'clicked', G_CALLBACK(@updateUserData), nil);
        gtk_table_attach(GTK_TABLE(grid), btnGuardarCambios, 0, 2, 2, 3, GTK_SHRINK, GTK_SHRINK, 5, 5);

        gtk_widget_show_all(actualizarDatosWindow);
        g_signal_connect(actualizarDatosWindow, 'destroy', G_CALLBACK(@gtk_main_quit), nil);
        gtk_main;
    end;

end.
