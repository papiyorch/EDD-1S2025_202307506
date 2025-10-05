unit verBorradores;

{$MODE DELPHI}

interface
uses 
    SysUtils, gtk2, glib2, gdk2, ArbolAVL, InterfaceTools, variables, ListaDoble, ListaSimple, ListaCircular;

    procedure showVerBorradoresWindow;
    procedure insertarBorrador(borrador: TBorrador);

implementation

    var
        verBorradoresWindow, modificarBorradorWindow: PGtkWidget;
        treeView: PGtkWidget;
        listStore: PGtkListStore;
        destinatarioEntry, asuntoEntry, mensajeEntry: PGtkWidget;

    procedure AgregarBorrador(borrador: TBorrador);
    var
        iter: TGtkTreeIter;
    begin
        gtk_list_store_append(listStore, @iter);
        gtk_list_store_set(listStore, @iter,
            0, PChar(borrador.idCorreo),
            1, PChar(borrador.asunto),
            2, PChar(borrador.remitente),
            -1);
    end;

    procedure insertarBorrador(borrador: TBorrador);
    begin
        if borrador.idCorreo = '' then
        begin
            mostrarMensajeError(verBorradoresWindow, 'Error', 'No se puede agregar un borrador sin ID.');
            Exit;
        end;

        usuarioActual^.borradores := insertarAVL(usuarioActual^.borradores, StrToInt(borrador.idCorreo), borrador);
        mostrarMensajeLogin(verBorradoresWindow, 'Éxito', 'Borrador agregado correctamente.');

        if listStore <> nil then
        begin
            gtk_list_store_clear(listStore);
            recorrerInorden(usuarioActual^.borradores, @AgregarBorrador);
        end;
    end;   

    procedure modificarBorrador(widget: PGtkWidget; data: gpointer); cdecl;
    var
        borrador: PNodoAVL;
        correoDestinatario, asunto, mensaje: String;
    begin
        correoDestinatario := Trim(gtk_entry_get_text(GTK_ENTRY(destinatarioEntry)));
        asunto := Trim(gtk_entry_get_text(GTK_ENTRY(asuntoEntry)));
        mensaje := Trim(gtk_entry_get_text(GTK_ENTRY(mensajeEntry)));   
        
        //Actualizar el borrador
        borrador := PNodoAVL(data);
        if borrador <> nil then
        begin
            borrador^.valor.destinatario := correoDestinatario;
            borrador^.valor.asunto := asunto;
            borrador^.valor.mensaje := mensaje;
            mostrarMensajeLogin(modificarBorradorWindow, 'Éxito', 'Borrador modificado correctamente.');
            gtk_widget_destroy(modificarBorradorWindow);
        end
        else
            mostrarMensajeError(modificarBorradorWindow, 'Error', 'No se pudo modificar el borrador.');
    end;

    procedure onEnviarClicked(button: PGtkWidget; data: gpointer); cdecl;
    var
        borrador: PNodoAVL;
        contactoDestino: PNodo;
    begin
        borrador := PNodoAVL(data);
        if borrador = nil then
        begin
            mostrarMensajeError(verBorradoresWindow, 'Error', 'No se encontró el borrador para enviar.');
            Exit;
        end;

        // Llamar a enviarCorreo con los datos del borrador
        contactoDestino := buscarUsuarioPorEmail(listaUsuarios, borrador^.valor.destinatario);
        if contactoDestino = nil then
        begin
            mostrarMensajeError(verBorradoresWindow, 'Error', 'No se encontró el usuario destino.');
            Exit;
        end;
        insertarDoble(contactoDestino^.correos, borrador^.valor.idCorreo, borrador^.valor.remitente, 'NL', False, borrador^.valor.asunto, Now, borrador^.valor.mensaje);
        mostrarMensajeLogin(verBorradoresWindow, 'Éxito', 'Correo enviado correctamente.');

        // Eliminar el borrador del árbol AVL
        eliminar(usuarioActual^.borradores, borrador^.clave);
        gtk_list_store_clear(listStore);
        recorrerInorden(usuarioActual^.borradores, @AgregarBorrador);
    end;

    procedure mostrarVentanaEdicion(idCorreo: Integer);
    var
        grid: PGtkWidget;
        btnEnviar: PGtkWidget;
        btnGuardar: PGtkWidget;
        borrador: PNodoAVL;
        buffer: PGtkTextBuffer;
    begin
        borrador := buscar(usuarioActual^.borradores, idCorreo);
        if borrador = nil then
        begin
            mostrarMensajeError(verBorradoresWindow, 'Error', 'No se encontró el borrador.');
            Exit;
        end;

        gtk_init(@argc, @argv);

        modificarBorradorWindow := gtk_window_new(GTK_WINDOW_TOPLEVEL);
        gtk_window_set_title(GTK_WINDOW(modificarBorradorWindow), 'Modificar Borrador');
        gtk_container_set_border_width(GTK_CONTAINER(modificarBorradorWindow), 10);
        gtk_window_set_default_size(GTK_WINDOW(modificarBorradorWindow), 400, 300);

        grid := gtk_table_new(5, 2, False);
        gtk_container_add(GTK_CONTAINER(modificarBorradorWindow), grid);

        destinatarioEntry := gtk_entry_new();
        gtk_entry_set_text(GTK_ENTRY(destinatarioEntry), PChar(borrador^.valor.destinatario));

        asuntoEntry := gtk_entry_new();
        gtk_entry_set_text(GTK_ENTRY(asuntoEntry), PChar(borrador^.valor.asunto));

        mensajeEntry := gtk_entry_new();
        gtk_entry_set_text(GTK_ENTRY(mensajeEntry), PChar(borrador^.valor.mensaje));

        gtk_table_attach_defaults(GTK_TABLE(grid), gtk_label_new('Destinatario:'), 0, 1, 0, 1);
        gtk_table_attach_defaults(GTK_TABLE(grid), destinatarioEntry, 1, 2, 0, 1);
        gtk_table_attach_defaults(GTK_TABLE(grid), gtk_label_new('Asunto:'), 0, 1, 1, 2);
        gtk_table_attach_defaults(GTK_TABLE(grid), asuntoEntry, 1, 2, 1, 2);
        gtk_table_attach_defaults(GTK_TABLE(grid), gtk_label_new('Mensaje:'), 0, 1, 2, 3);
        gtk_table_attach_defaults(GTK_TABLE(grid), mensajeEntry, 1, 2, 2, 3);

        btnGuardar := gtk_button_new_with_label('Guardar Cambios');    
        g_signal_connect(btnGuardar, 'clicked', G_CALLBACK(@modificarBorrador), gpointer(borrador));
        gtk_table_attach_defaults(GTK_TABLE(grid), btnGuardar, 0, 2, 4, 5);

        btnEnviar := gtk_button_new_with_label('Enviar Correo');
        g_signal_connect(btnEnviar, 'clicked', G_CALLBACK(@onEnviarClicked), gpointer(borrador));
        gtk_table_attach_defaults(GTK_TABLE(grid), btnEnviar, 0, 2, 5, 6);

        gtk_widget_show_all(modificarBorradorWindow);

        g_signal_connect(modificarBorradorWindow, 'destroy', G_CALLBACK(@gtk_main_quit), nil);
        gtk_main;
    end;

    procedure onTreeViewRowActivated(treeView: PGtkTreeView; path: PGtkTreePath; column: PGtkTreeViewColumn; data: gpointer); cdecl;
    var
        model: PGtkTreeModel;
        iter: TGtkTreeIter;
        idCorreo: PChar;
    begin
        model := gtk_tree_view_get_model(GTK_TREE_VIEW(treeView));

        if gtk_tree_model_get_iter(model, @iter, path) then
        begin
            gtk_tree_model_get(model, @iter, 0, @idCorreo, -1);
            if idCorreo <> nil then
            begin
                mostrarVentanaEdicion(StrToInt(idCorreo));
                g_free(idCorreo);
            end;
        end;
    end;

    procedure showVerBorradoresWindow;
    var
        grid: PGtkWidget;
        colId, colAsunto, colRemitente: PGtkTreeViewColumn;
    begin
        gtk_init(@argc, @argv);

        verBorradoresWindow := gtk_window_new(GTK_WINDOW_TOPLEVEL);
        gtk_window_set_title(GTK_WINDOW(verBorradoresWindow), 'Ver Borradores');
        gtk_container_set_border_width(GTK_CONTAINER(verBorradoresWindow), 10);
        gtk_window_set_default_size(GTK_WINDOW(verBorradoresWindow), 600, 400);

        grid := gtk_table_new(2, 1, False);
        gtk_container_add(GTK_CONTAINER(verBorradoresWindow), grid);

        //Crear TreeView y ListStore
        listStore := gtk_list_store_new(3, G_TYPE_STRING, G_TYPE_STRING, G_TYPE_STRING);
        treeView := gtk_tree_view_new_with_model(GTK_TREE_MODEL(listStore));

        //Crear columnas
        colId := gtk_tree_view_column_new_with_attributes('ID', gtk_cell_renderer_text_new(), 'text', 0, nil);
        colAsunto := gtk_tree_view_column_new_with_attributes('Asunto', gtk_cell_renderer_text_new(), 'text', 1, nil);
        colRemitente := gtk_tree_view_column_new_with_attributes('Remitente', gtk_cell_renderer_text_new(), 'text', 2, nil);

        gtk_tree_view_append_column(GTK_TREE_VIEW(treeView), colId);
        gtk_tree_view_append_column(GTK_TREE_VIEW(treeView), colAsunto);
        gtk_tree_view_append_column(GTK_TREE_VIEW(treeView), colRemitente);

        gtk_table_attach_defaults(GTK_TABLE(grid), treeView, 0, 1, 0, 1);

        //Llenar el ListStore con los borradores existentes
        gtk_list_store_clear(listStore);
        recorrerInorden(usuarioActual^.borradores, @AgregarBorrador);

        //Conectar la señal de activación de fila
        g_signal_connect(G_OBJECT(treeView), 'row-activated', G_CALLBACK(@onTreeViewRowActivated), nil);
        g_signal_connect(verBorradoresWindow, 'destroy', G_CALLBACK(@gtk_main_quit), nil);

        gtk_widget_show_all(verBorradoresWindow);
        gtk_main;
    end;
end.
