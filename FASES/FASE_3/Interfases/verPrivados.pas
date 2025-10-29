unit verPrivados;

{$MODE DELPHI}

interface
    uses 
        SysUtils, gtk2, glib2, gdk2, ArbolMerkle, InterfaceTools, variables, ListaSimple, ListaDoble;

    procedure showVerPrivadosWindow;
    procedure insertarPrivado(privado: PCorreo);

implementation

    var
        verPrivadosWindow, detalleWindow: PGtkWidget;
        treeView: PGtkWidget;
        listStore: PGtkListStore;
        merkleTree: TMrklePrivados;

    procedure AgregarPrivado(correos: PCorreo);
    var
        iter: TGtkTreeIter;
    begin
        Writeln('Agregando al listStore: ', correos^.idCorreo, ' | ', correos^.asunto, ' | ', correos^.remitente);
        gtk_list_store_append(listStore, @iter);
        gtk_list_store_set(listStore, @iter,
            0, PChar(correos^.idCorreo),
            1, PChar(correos^.asunto),
            2, PChar(correos^.remitente),
            -1);
    end;

    procedure insertarPrivado(privado: PCorreo);
    begin
        if merkleTree = nil then
            merkleTree := TMrklePrivados.Create;

        merkleTree.Insert(privado);
        AgregarPrivado(privado);
    end;

    procedure eliminarPrivado(widget: PGtkWidget; data: gpointer); cdecl;
    var
        idCorreo: string;
    begin
        idCorreo := PChar(data);

        if (merkleTree <> nil) and (merkleTree.FindById(idCorreo) <> nil) then
        begin
            merkleTree.DeleteById(idCorreo);
            mostrarMensajeLogin(verPrivadosWindow, 'Eliminar Privado', 'El correo privado ha sido eliminado.');
        end
        else
            mostrarMensajeLogin(verPrivadosWindow, 'Eliminar Privado', 'El correo no se encontró.');
    end;

    function obtenerPrivado(idCorreo: string): PCorreo;
    var
        Node: TMrkleNode;
    begin
        Result := nil;
        if merkleTree <> nil then
        begin
            Node := merkleTree.FindById(idCorreo);
            if Node <> nil then
                Result := Node.Privado;
        end;
    end;

    procedure mostrarDetallePrivado(correo: PCorreo);
    var
        grid, btnEliminar: PGtkWidget;
        idPtr: PChar;
    begin
        if correo = nil then
        begin
            Writeln('Error: El correo es nil.');
            Exit;
        end;

        Writeln('Mostrando detalles del correo: ', correo^.idCorreo);
        detalleWindow := gtk_window_new(GTK_WINDOW_TOPLEVEL);
        gtk_window_set_title(GTK_WINDOW(detalleWindow), 'Detalle del Correo Privado');
        gtk_container_set_border_width(GTK_CONTAINER(detalleWindow), 10);
        gtk_window_set_default_size(GTK_WINDOW(detalleWindow), 400, 300);

        grid := gtk_table_new(6, 2, False);
        gtk_container_add(GTK_CONTAINER(detalleWindow), grid);

        gtk_table_attach_defaults(GTK_TABLE(grid), gtk_label_new('Remitente:'), 0, 1, 0, 1);
        gtk_table_attach_defaults(GTK_TABLE(grid), gtk_label_new(PChar(correo^.remitente)), 1, 2, 0, 1);
        gtk_table_attach_defaults(GTK_TABLE(grid), gtk_label_new('Asunto:'), 0, 1, 1, 2);
        gtk_table_attach_defaults(GTK_TABLE(grid), gtk_label_new(PChar(correo^.asunto)), 1, 2, 1, 2);
        gtk_table_attach_defaults(GTK_TABLE(grid), gtk_label_new('Fecha:'), 0, 1, 2, 3);
        gtk_table_attach_defaults(GTK_TABLE(grid), gtk_label_new(PChar(correo^.fecha)), 1, 2, 2, 3);
        gtk_table_attach_defaults(GTK_TABLE(grid), gtk_label_new('Mensaje:'), 0, 1, 3, 4);
        gtk_table_attach_defaults(GTK_TABLE(grid), gtk_label_new(PChar(correo^.mensaje)), 1, 2, 3, 4);

        btnEliminar := gtk_button_new_with_label('Eliminar');

        idPtr := PChar(correo^.idCorreo);
        g_signal_connect(btnEliminar, 'clicked', G_CALLBACK(@eliminarPrivado), gpointer(idPtr));

        gtk_table_attach_defaults(GTK_TABLE(grid), btnEliminar, 0, 2, 4, 5);

        gtk_widget_show_all(detalleWindow);
    end;

    procedure onPrivadoSeleccionado(treeView: PGtkWidget; path: PGtkTreePath; column: PGtkTreeViewColumn; user_data: gpointer); cdecl;
    var
        model: PGtkTreeModel;
        iter: TGtkTreeIter;
        claveSTR: PChar;
        privado: PCorreo;
    begin
        Writeln('Evento row-activated disparado.');
        model := gtk_tree_view_get_model(GTK_TREE_VIEW(treeView));
        if gtk_tree_model_get_iter(model, @iter, path) then
        begin
            gtk_tree_model_get(model, @iter, 0, @claveSTR, -1);
            Writeln('ID seleccionado: ', String(claveSTR));
            privado := obtenerPrivado(String(claveSTR));

            if privado <> nil then
            begin
                Writeln('Correo encontrado. Mostrando detalles...');
                mostrarDetallePrivado(privado);
            end
            else
                Writeln('Error: Correo no encontrado.');
        end
        else
            Writeln('Error: No se pudo obtener el iterador del modelo.');
    end;

    
    procedure LlenarTablaPrivados;
    var
        hojas: TArrayOfPCorreo;
        i: Integer;
        begin
        if usuarioActual^.privados = nil then Exit;

        hojas := usuarioActual^.privados.GetLeaves;
        for i := 0 to Length(hojas) - 1 do
        begin
            if hojas[i] <> nil then
            AgregarPrivado(hojas[i]);
        end;
    end;

    procedure showVerPrivadosWindow;
    var
        grid: PGtkWidget;
        colId, colAsunto, colRemitente: PGtkTreeViewColumn;
        i: Integer;
        Node: TMrkleNode;
    begin
        gtk_init(@argc, @argv);

        verPrivadosWindow := gtk_window_new(GTK_WINDOW_TOPLEVEL);
        gtk_window_set_title(GTK_WINDOW(verPrivadosWindow), 'Correos Privados');
        gtk_container_set_border_width(GTK_CONTAINER(verPrivadosWindow), 10);
        gtk_window_set_default_size(GTK_WINDOW(verPrivadosWindow), 600, 400);

        grid := gtk_table_new(2, 1, False);
        gtk_container_add(GTK_CONTAINER(verPrivadosWindow), grid);

        listStore := gtk_list_store_new(3, G_TYPE_STRING, G_TYPE_STRING, G_TYPE_STRING);
        treeView := gtk_tree_view_new_with_model(GTK_TREE_MODEL(listStore));

        colId := gtk_tree_view_column_new_with_attributes('ID', gtk_cell_renderer_text_new(), 'text', 0, nil);
        gtk_tree_view_append_column(GTK_TREE_VIEW(treeView), GTK_TREE_VIEW_COLUMN(colId));

        colAsunto := gtk_tree_view_column_new_with_attributes('Asunto', gtk_cell_renderer_text_new(), 'text', 1, nil);
        gtk_tree_view_append_column(GTK_TREE_VIEW(treeView), GTK_TREE_VIEW_COLUMN(colAsunto));

        colRemitente := gtk_tree_view_column_new_with_attributes('Remitente', gtk_cell_renderer_text_new(), 'text', 2, nil);
        gtk_tree_view_append_column(GTK_TREE_VIEW(treeView), GTK_TREE_VIEW_COLUMN(colRemitente));

        gtk_table_attach_defaults(GTK_TABLE(grid), treeView, 0, 1, 0, 1);

        gtk_list_store_clear(listStore);
        Writeln('Llenando tabla de privados...');
        LlenarTablaPrivados;

        g_signal_connect(G_OBJECT(treeView), 'row-activated', G_CALLBACK(@onPrivadoSeleccionado), nil);
        g_signal_connect(verPrivadosWindow, 'destroy', G_CALLBACK(@gtk_main_quit), nil);

        gtk_widget_show_all(verPrivadosWindow);
        gtk_main;
    end;
end.