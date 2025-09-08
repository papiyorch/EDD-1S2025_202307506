unit perfilUsuario;

{$MODE DELPHI}

interface
    procedure showPerfilUsuarioWindow;

implementation
    uses
        SysUtils, Classes, InterfaceTools, ListaSimple, gtk2, glib2, gdk2, variables, actualizarDatos;

    var
        perfilUsuarioWindow: PGtkWidget;
        treeView: PGtkWidget;
        listStore: PGtkListStore;
        btnActualizar: PGtkWidget;

    procedure onActualizarDatosClicked(widget: PGtkWidget; data: gpointer); cdecl;
    begin
        showActualizarDatosWindow;
    end;

    procedure showPerfilUsuarioWindow;
    var
        grid: PGtkWidget;
        actual: PNodo;
        iter: TGtkTreeIter;
        colKey, colValue: PGtkTreeViewColumn;

    begin
        gtk_init(@argc, @argv);

        perfilUsuarioWindow := gtk_window_new(GTK_WINDOW_TOPLEVEL);
        gtk_window_set_title(GTK_WINDOW(perfilUsuarioWindow), 'Perfil de Usuario');
        gtk_container_set_border_width(GTK_CONTAINER(perfilUsuarioWindow), 10);
        gtk_window_set_default_size(GTK_WINDOW(perfilUsuarioWindow), 600, 400);

        grid := gtk_table_new(2, 1, False);
        gtk_container_add(GTK_CONTAINER(perfilUsuarioWindow), grid);

        listStore := gtk_list_store_new(2, G_TYPE_STRING, G_TYPE_STRING);
        treeView := gtk_tree_view_new_with_model(GTK_TREE_MODEL(listStore));
        gtk_widget_set_size_request(treeView, 350, 120);

        //Columnas de llave y valor
        colKey := gtk_tree_view_column_new_with_attributes('Campo', gtk_cell_renderer_text_new(), 'text', 0, nil);
        colValue := gtk_tree_view_column_new_with_attributes('Información', gtk_cell_renderer_text_new(), 'text', 1, nil);

        gtk_tree_view_append_column(GTK_TREE_VIEW(treeView), colKey);
        gtk_tree_view_append_column(GTK_TREE_VIEW(treeView), colValue);

        gtk_tree_view_column_set_sizing(colKey, GTK_TREE_VIEW_COLUMN_FIXED);
        gtk_tree_view_column_set_fixed_width(colKey, 170);

        gtk_tree_view_column_set_sizing(colValue, GTK_TREE_VIEW_COLUMN_FIXED);
        gtk_tree_view_column_set_fixed_width(colValue, 170);

        gtk_table_attach_defaults(GTK_TABLE(grid), treeView, 0, 1, 0, 1);

        //Agregar datos del usuario actual
        actual := usuarioActual;
        if actual <> nil then
        begin
            gtk_list_store_append(GTK_LIST_STORE(listStore), @iter);
            gtk_list_store_set(GTK_LIST_STORE(listStore), @iter, 0, PChar('Nombre'), 1, PChar(actual^.nombre), -1);

            gtk_list_store_append(GTK_LIST_STORE(listStore), @iter);
            gtk_list_store_set(GTK_LIST_STORE(listStore), @iter, 0, PChar('Usuario'), 1, PChar(actual^.usuario), -1);

            gtk_list_store_append(GTK_LIST_STORE(listStore), @iter);
            gtk_list_store_set(GTK_LIST_STORE(listStore), @iter, 0, PChar('Correo'), 1, PChar(actual^.email), -1);

            gtk_list_store_append(GTK_LIST_STORE(listStore), @iter);
            gtk_list_store_set(GTK_LIST_STORE(listStore), @iter, 0, PChar('Teléfono'), 1, PChar(actual^.telefono), -1);
        end;

        g_signal_connect(perfilUsuarioWindow, 'destroy', G_CALLBACK(@gtk_main_quit), nil);

        btnActualizar := gtk_button_new_with_label('Actualizar Datos');
        g_signal_connect(btnActualizar, 'clicked', G_CALLBACK(@onActualizarDatosClicked), nil);
        gtk_table_attach(GTK_TABLE(grid), btnActualizar, 0, 1, 1, 2, GTK_SHRINK, GTK_SHRINK, 7, 7);

        gtk_widget_show_all(perfilUsuarioWindow);
        gtk_main;
    end;
end.