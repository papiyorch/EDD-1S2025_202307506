unit contactosInfo;

interface
    procedure showContactInfoWindow;

implementation
    uses
        SysUtils, gtk2, glib2, gdk2, variables, ListaCircular;

    var
        ventanaInfor: PGtkWidget;
        labelNombre, labelUsuario, labelCorreo, labelTelefono: PGtkWidget;
        btnSiguiente: PGtkWidget;
        actualContacto: PNode;

    procedure mostrarContacto;
    begin
        if actualContacto <> nil then
        begin
            gtk_label_set_text(GTK_LABEL(labelNombre), PChar('Nombre: ' + actualContacto^.nombre));
            gtk_label_set_text(GTK_LABEL(labelUsuario), PChar('Usuario: ' + actualContacto^.usuario));
            gtk_label_set_text(GTK_LABEL(labelCorreo), PChar('Correo: ' + actualContacto^.email));
            gtk_label_set_text(GTK_LABEL(labelTelefono), PChar('Teléfono: ' + actualContacto^.telefono));
        end;
    end;

    procedure siguienteContacto(widget: PGtkWidget; data: gpointer); cdecl;
    begin
        if (actualContacto <> nil) and (actualContacto^.next <> nil) then
            actualContacto := actualContacto^.next;
        mostrarContacto;
    end;

    procedure showContactInfoWindow;
    var
        grid: PGtkWidget;
    begin
        ventanaInfor := gtk_window_new(GTK_WINDOW_TOPLEVEL);
        gtk_window_set_title(GTK_WINDOW(ventanaInfor), 'Información de Contacto');
        gtk_container_set_border_width(GTK_CONTAINER(ventanaInfor), 10);
        gtk_window_set_default_size(GTK_WINDOW(ventanaInfor), 300, 200);

        grid := gtk_table_new(5, 1, False);
        gtk_container_add(GTK_CONTAINER(ventanaInfor), grid);

        labelNombre := gtk_label_new('');
        labelUsuario := gtk_label_new('');
        labelCorreo := gtk_label_new('');
        labelTelefono := gtk_label_new('');

        gtk_table_attach_defaults(GTK_TABLE(grid), labelNombre, 0, 1, 0, 1);
        gtk_table_attach_defaults(GTK_TABLE(grid), labelUsuario, 0, 1, 1, 2);
        gtk_table_attach_defaults(GTK_TABLE(grid), labelCorreo, 0, 1, 2, 3);
        gtk_table_attach_defaults(GTK_TABLE(grid), labelTelefono, 0, 1, 3, 4);

        btnSiguiente := gtk_button_new_with_label('Siguiente');
        g_signal_connect(btnSiguiente, 'clicked', G_CALLBACK(@siguienteContacto), nil);
        gtk_table_attach_defaults(GTK_TABLE(grid), btnSiguiente, 0, 1, 4, 5);

        actualContacto := head; 
        mostrarContacto;

        gtk_widget_show_all(ventanaInfor);

        g_signal_connect(ventanaInfor, 'destroy', G_CALLBACK(@gtk_widget_destroy), ventanaInfor);
    end;
end.