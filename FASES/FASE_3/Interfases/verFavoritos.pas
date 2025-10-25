unit verFavoritos;

{$MODE DELPHI}

interface
uses
    SysUtils, gtk2, glib2, gdk2, ArbolB, InterfaceTools, variables, ListaSimple;

    procedure showVerFavoritosWindow;
    procedure insertarFavorito(correo: TCorreoFavorito);

implementation

    var
        verFavoritosWindow, detalleWindow: PGtkWidget;
        treeView: PGtkWidget;
        listStore: PGtkListStore;

    procedure AgregarFavorito(correos: TCorreoFavorito);
    var
        iter: TGtkTreeIter;
    begin
        gtk_list_store_append(listStore, @iter);
        gtk_list_store_set(listStore, @iter,
            0, PChar(correos.idCorreo),
            1, PChar(correos.asunto),
            2, PChar(correos.remitente),
            -1);
    end;

    procedure insertarFavorito(correo: TCorreoFavorito);
    begin
        if correo.idCorreo = '' then
        begin
            mostrarMensajeError(verFavoritosWindow, 'Error', 'No se puede agregar un favorito sin ID.');
            Exit;
        end;

        insertarB(usuarioActual^.favoritos, StrToInt(correo.idCorreo), correo);

        mostrarMensajeLogin(verFavoritosWindow, 'Éxito', 'Favorito agregado correctamente.');
        
        if listStore <> nil then
        begin
            gtk_list_store_clear(listStore);
            recorrerInorden(usuarioActual^.favoritos, @AgregarFavorito);
        end;
    end;
    
    procedure eliminarFavorito(widget: PGtkWidget; data: gpointer); cdecl;
    var
        selection: PGtkTreeSelection;
        model: PGtkTreeModel;
        iter: TGtkTreeIter;
        claveStr: PChar;
        clave: Integer;
    begin
        if usuarioActual^.favoritos = nil then
        begin
            mostrarMensajeError(verFavoritosWindow, 'Error', 'No hay favoritos para eliminar.');
            Exit;
        end;

        selection := gtk_tree_view_get_selection(GTK_TREE_VIEW(treeView));
        if gtk_tree_selection_get_selected(selection, @model, @iter) then
        begin
            gtk_tree_model_get(model, @iter, 0, @claveStr, -1);
            clave := StrToInt(claveStr);
            eliminar(usuarioActual^.favoritos, clave);
            mostrarMensajeLogin(verFavoritosWindow, 'Éxito', 'Favorito eliminado correctamente.');

            //refrescar la vista
            gtk_list_store_clear(listStore);
            recorrerInorden(usuarioActual^.favoritos, @AgregarFavorito);
        end
        else
            mostrarMensajeError(verFavoritosWindow, 'Error', 'No se ha seleccionado ningún favorito para eliminar.');
    end;

    function obtenerFavorito(clave: Integer): TCorreoFavorito;
    var
        nodo: PNodoB;
        i: Integer;
        
    begin
        nodo := buscar(usuarioActual^.favoritos, clave);
        if nodo = nil then
            mostrarMensajeError(verFavoritosWindow, 'Error', 'No se encontró el favorito.');

        for i := 0 to nodo^.id - 1 do
            if nodo^.claves[i] = clave then
            begin
                Result := nodo.valores[i];
                Exit;
            end;
        mostrarMensajeError(verFavoritosWindow, 'Error', 'No se encontró el favorito dentro del nodo.');
    end;

    procedure mostrarDetalleFavorito(correo: TCorreoFavorito);
    var
        grid, btnEliminar: PGtkWidget;
    begin
        detalleWindow := gtk_window_new(GTK_WINDOW_TOPLEVEL);
        gtk_window_set_title(GTK_WINDOW(detalleWindow), 'Detalle del Correo');
        gtk_container_set_border_width(GTK_CONTAINER(detalleWindow), 10);
        gtk_window_set_default_size(GTK_WINDOW(detalleWindow), 400, 300);

        grid := gtk_table_new(5, 2, False);
        gtk_container_add(GTK_CONTAINER(detalleWindow), grid);

        
        gtk_table_attach_defaults(GTK_TABLE(grid), gtk_label_new('Remitente:'), 0, 1, 0, 1);
        gtk_table_attach_defaults(GTK_TABLE(grid), gtk_label_new(PChar(correo.remitente)), 1, 2, 0, 1);
        gtk_table_attach_defaults(GTK_TABLE(grid), gtk_label_new('Asunto:'), 0, 1, 1, 2);
        gtk_table_attach_defaults(GTK_TABLE(grid), gtk_label_new(PChar(correo.asunto)), 1, 2, 1, 2);
        gtk_table_attach_defaults(GTK_TABLE(grid), gtk_label_new('Destinatario:'), 0, 1, 2, 3);
        gtk_table_attach_defaults(GTK_TABLE(grid), gtk_label_new(PChar(correo.destinatario)), 1, 2, 2, 3);
        gtk_table_attach_defaults(GTK_TABLE(grid), gtk_label_new('Mensaje:'), 0, 1, 3, 4);
        gtk_table_attach_defaults(GTK_TABLE(grid), gtk_label_new(PChar(correo.mensaje)), 1, 2, 3, 4);

        btnEliminar := gtk_button_new_with_label('Eliminar');
        g_signal_connect(btnEliminar, 'clicked', G_CALLBACK(@eliminarFavorito), gpointer(correo.idCorreo));
        gtk_table_attach_defaults(GTK_TABLE(grid), btnEliminar, 0, 2, 4, 5);

        gtk_widget_show_all(detalleWindow);
    end;
        
    procedure onFavoritoSeleccionado(treeView: PGtkWidget; path: PGtkTreePath; column: PGtkTreeViewColumn; user_data: gpointer); cdecl;
    var
        model: PGtkTreeModel;
        iter: TGtkTreeIter;
        claveStr: PChar;
        clave: Integer;
        correo: TCorreoFavorito;
    begin
        model := gtk_tree_view_get_model(GTK_TREE_VIEW(treeView));

        if gtk_tree_model_get_iter(model, @iter, path) then
        begin
            gtk_tree_model_get(model, @iter, 0, @claveStr, -1); 
            clave := StrToInt(claveStr);

            correo := obtenerFavorito(clave);
            mostrarDetalleFavorito(correo);
        end;
    end;

    procedure showVerFavoritosWindow;
    var
        grid: PGtkWidget;
        colClave, colAsunto, colRemitente: PGtkTreeViewColumn;
    
    begin
        gtk_init(@argc, @argv);

        verFavoritosWindow := gtk_window_new(GTK_WINDOW_TOPLEVEL);
        gtk_window_set_title(GTK_WINDOW(verFavoritosWindow), 'Favoritos');
        gtk_container_set_border_width(GTK_CONTAINER(verFavoritosWindow), 10);
        gtk_window_set_default_size(GTK_WINDOW(verFavoritosWindow), 600, 400);

        grid := gtk_table_new(2, 1, False);
        gtk_container_add(GTK_CONTAINER(verFavoritosWindow), grid);

        // Crear el TreeView y el ListStore
        listStore := gtk_list_store_new(3, G_TYPE_STRING, G_TYPE_STRING, G_TYPE_STRING);
        treeView := gtk_tree_view_new_with_model(GTK_TREE_MODEL(listStore));

        // Crear columnas
        colClave := gtk_tree_view_column_new_with_attributes('Clave', gtk_cell_renderer_text_new(), 'text', 0, nil);
        colAsunto := gtk_tree_view_column_new_with_attributes('Asunto', gtk_cell_renderer_text_new(), 'text', 1, nil);
        colRemitente := gtk_tree_view_column_new_with_attributes('Remitente', gtk_cell_renderer_text_new(), 'text', 2, nil);

        // Añadir columnas al TreeView
        gtk_tree_view_append_column(GTK_TREE_VIEW(treeView), colClave);
        gtk_tree_view_append_column(GTK_TREE_VIEW(treeView), colAsunto);
        gtk_tree_view_append_column(GTK_TREE_VIEW(treeView), colRemitente);

        //Agregar el TreeView al grid
        gtk_table_attach_defaults(GTK_TABLE(grid), treeView, 0, 1, 0, 1);

        // Rellenar el ListStore con los favoritos del usuario actual
        gtk_list_store_clear(listStore);
        recorrerInorden(usuarioActual^.favoritos, @AgregarFavorito);

        // Conectar la señal de selección
        g_signal_connect(G_OBJECT(treeView), 'row-activated', G_CALLBACK(@onFavoritoSeleccionado), nil);
        g_signal_connect(verFavoritosWindow, 'destroy', G_CALLBACK(@gtk_main_quit), nil);

        gtk_widget_show_all(verFavoritosWindow);
        gtk_main;
    end;
end.

