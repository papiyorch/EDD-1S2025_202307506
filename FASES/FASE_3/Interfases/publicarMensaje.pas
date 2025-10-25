unit publicarMensaje;

{$MODE DELPHI}

interface

    procedure showPublicarMensajeWindow;

implementation
    uses 
        SysUtils, gtk2, glib2, gdk2, variables, listaDeLista, Comunidades, InterfaceTools, listaSimpleMensajes, AbolBST;
    var
        publicarMensajeWindow: PGtkWidget;
        entryMensaje, comboComunidades: PGtkWidget;
        btnPublicar: PGtkWidget;
        contadorMensajes: Integer = 0;

    procedure limpiarCombo;
    var
        count, i: Integer;
    begin
        count := gtk_tree_model_iter_n_children(gtk_combo_box_get_model(GTK_COMBO_BOX(comboComunidades)), nil);
        for i := count-1 downto 0 do
            gtk_combo_box_remove_text(GTK_COMBO_BOX(comboComunidades), i);
    end;

    procedure actualizarComboComunidadesPublicar;
    var
        comunidadActual: PComunidad;
    begin
        limpiarCombo;
        comunidadActual := listaComunidades.cabeza;
        while comunidadActual <> nil do
        begin
            gtk_combo_box_append_text(GTK_COMBO_BOX(comboComunidades), PChar(comunidadActual^.nombre));
            comunidadActual := comunidadActual^.siguiente;
        end;
    end;

    procedure publicarMensaje(widget: PGtkWidget; data: gpointer); cdecl;
    var
        mensaje: String;
        comunidadSeleccionada: PChar;
        comunidad: PComunidad;
        valor: TArbolBST;
        nodo: PNodoBST;
    begin
        mensaje := Trim(gtk_entry_get_text(GTK_ENTRY(entryMensaje)));
        comunidadSeleccionada := gtk_combo_box_get_active_text(GTK_COMBO_BOX(comboComunidades));
        Inc(contadorMensajes);
        // Validaciones
        if (mensaje = '') or (comunidadSeleccionada = nil) then
        begin
            mostrarMensajeError(publicarMensajeWindow, 'Error', 'Debe ingresar un mensaje y seleccionar una comunidad.');
            Exit;
        end;

        comunidad := comunidadPorNombre(listaComunidades, String(comunidadSeleccionada));
        if comunidad = nil then
        begin
            mostrarMensajeError(publicarMensajeWindow, 'Error', 'La comunidad seleccionada no existe.');
            Exit;
        end;

        if existeUsuarioEnComunidad(comunidad^.usuarios, usuarioActual^.email) = False then
        begin
            mostrarMensajeError(publicarMensajeWindow, 'Error', 'No pertenece a la comunidad seleccionada.');
            Exit;
        end;
        insertarMensaje(comunidad^.mensajes, IntToStr(contadorMensajes), usuarioActual^.email, mensaje, String(comunidadSeleccionada));
        //Actualizar el árbol de mensajes
        valor.nombreComunidad := comunidad^.nombre;
        valor.fechaCreacion := Now;
        valor.contadorMensajes := contadorMensajes;

        nodo := buscarNodoBST(raiz, String(comunidadSeleccionada));
        if nodo <> nil then
            nodo^.valor.contadorMensajes := nodo^.valor.contadorMensajes + 1
        else
            begin
                valor.contadorMensajes := 1;
                valor.fechaCreacion := Now;
                insertarNodoBST(raiz, String(comunidadSeleccionada), valor);
            end;

        mostrarMensajeLogin(publicarMensajeWindow, 'Éxito', 'Mensaje publicado en la comunidad.');
        gtk_entry_set_text(GTK_ENTRY(entryMensaje), '');
        imprimirMensajes(comunidad^.mensajes);
    end;

    procedure showPublicarMensajeWindow;
    var
        grid: PGtkWidget;
    begin
        gtk_init(@argc, @argv);

        publicarMensajeWindow := gtk_window_new(GTK_WINDOW_TOPLEVEL);
        gtk_window_set_title(GTK_WINDOW(publicarMensajeWindow), 'Publicar Mensaje');
        gtk_container_set_border_width(GTK_CONTAINER(publicarMensajeWindow), 10);
        gtk_window_set_default_size(GTK_WINDOW(publicarMensajeWindow), 400, 200);

        grid := gtk_table_new(3, 2, False);
        gtk_container_add(GTK_CONTAINER(publicarMensajeWindow), grid);

        entryMensaje := gtk_entry_new();
        comboComunidades := gtk_combo_box_new_text();
        

        gtk_table_attach_defaults(GTK_TABLE(grid), gtk_label_new('Mensaje:'), 0, 1, 0, 1);
        gtk_table_attach_defaults(GTK_TABLE(grid), entryMensaje, 1, 2, 0, 1);

        gtk_table_attach_defaults(GTK_TABLE(grid), gtk_label_new('Comunidad:'), 0, 1, 1, 2);
        gtk_table_attach_defaults(GTK_TABLE(grid), comboComunidades, 1, 2, 1, 2);

        btnPublicar := gtk_button_new_with_label('Publicar');
        g_signal_connect(btnPublicar, 'clicked', G_CALLBACK(@publicarMensaje), nil);
        gtk_table_attach(GTK_TABLE(grid), btnPublicar, 1, 2, 2, 3, GTK_SHRINK, GTK_SHRINK, 5, 5);
        actualizarComboComunidadesPublicar;
        gtk_widget_show_all(publicarMensajeWindow);
        g_signal_connect(publicarMensajeWindow, 'destroy', G_CALLBACK(@gtk_main_quit), nil);
        gtk_main;
    end;
end.
