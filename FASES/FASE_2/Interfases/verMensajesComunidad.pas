unit verMensajesComunidad;

{$MODE DELPHI}

interface
    uses
        SysUtils, gtk2, glib2, gdk2, variables, InterfaceTools, listaDeLista, AbolBST;

    procedure showMensajesComunidadWindow;
implementation
    var
        verMensajesComunidadwindow: PGtkWidget;
        treeView: PGtkWidget;
        listStore: PGtkListStore;

    procedure AgregarNodoABST(data: TArbolBST);
    var
        iter: TGtkTreeIter;
    begin
        gtk_list_store_append(listStore, @iter);
        gtk_list_store_set(listStore, @iter,
            0, PChar(data.nombreComunidad),
            1, PChar(DateTimeToStr(data.fechaCreacion)),
            2, PChar(IntToStr(data.contadorMensajes)),
            -1);
        
    end;

    procedure showMensajesComunidadWindow;
    var
        grid: PGtkWidget;
        colComunidad, colFechaCreacion, colNumMnesajes: PGtkTreeViewColumn;
    begin
        gtk_init(@argc, @argv);

        verMensajesComunidadwindow := gtk_window_new(GTK_WINDOW_TOPLEVEL);
        gtk_window_set_title(GTK_WINDOW(verMensajesComunidadwindow), 'Mensajes de Comunidad');
        gtk_container_set_border_width(GTK_CONTAINER(verMensajesComunidadwindow), 10);
        gtk_window_set_default_size(GTK_WINDOW(verMensajesComunidadwindow), 600, 400);

        grid := gtk_table_new(2, 1, False);
        gtk_container_add(GTK_CONTAINER(verMensajesComunidadwindow), grid);

        // Crear el TreeView y el ListStore
        listStore := gtk_list_store_new(3, G_TYPE_STRING, G_TYPE_STRING, G_TYPE_STRING);
        treeView := gtk_tree_view_new_with_model(GTK_TREE_MODEL(listStore));

        // Crear y agregar columnas al treeView
        colComunidad := gtk_tree_view_column_new_with_attributes('Comunidad', gtk_cell_renderer_text_new(), 'text', 0, nil);
        colFechaCreacion := gtk_tree_view_column_new_with_attributes('Fecha Creación', gtk_cell_renderer_text_new(), 'text', 1, nil);
        colNumMnesajes := gtk_tree_view_column_new_with_attributes('Número de Mensajes', gtk_cell_renderer_text_new(), 'text', 2, nil);

        gtk_tree_view_append_column(GTK_TREE_VIEW(treeView), colComunidad);
        gtk_tree_view_append_column(GTK_TREE_VIEW(treeView), colFechaCreacion);
        gtk_tree_view_append_column(GTK_TREE_VIEW(treeView), colNumMnesajes);

        //Agregar el TreeView al grid
        gtk_table_attach_defaults(GTK_TABLE(grid), treeView, 0, 1, 0, 1);

        //Lenar el ListStore con los datos del árbol BST
        if raiz = nil then
        begin
            WriteLn('El árbol BST está vacío.');
        end
        else
        begin
            gtk_list_store_clear(listStore);
            recorrerInorden(raiz, @AgregarNodoABST);
        end;

        g_signal_connect(verMensajesComunidadwindow, 'destroy', G_CALLBACK(@gtk_main_quit), nil);
        gtk_widget_show_all(verMensajesComunidadwindow);
        gtk_main;
    end;
end.
        