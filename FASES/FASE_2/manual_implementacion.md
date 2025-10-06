# Manual de Implementación por Grupos
## EDDMail - Sistema de Gestión de Correo Electrónico

---

<div align="center">

**Universidad de San Carlos de Guatemala**  
**Facultad de Ingeniería**  
**Escuela de Ciencias y Sistemas**  
**Estructuras de Datos**  

---

**Proyecto:** EDDMail - Sistema de Gestión de Correo Electrónico  
**Fase 2 - Implementación por Grupos**  

**Integrantes del Grupo:**
- **Estudiante 1:** Manuel Alejandro López Canel - **Carné:** 202302035
- **Estudiante 2:** Jorge Ivan Samayoa Sian - **Carné:** 202307506  
- **Estudiante 3:** Allan Donaldo Xocop Tubac - **Carné:** 202300629 

**Fecha:** 5 de octubre de 2025  

</div>

---

## Índice

1. [Introducción](#introducción)
2. [Objetivos de la Implementación Grupal](#objetivos-de-la-implementación-grupal)
3. [Funcionalidad Asignada al Grupo](#funcionalidad-asignada-al-grupo)
4. [Diseño e Implementación](#diseño-e-implementación)
5. [Resultados Obtenidos](#resultados-obtenidos)

---

## Introducción

El presente manual documenta la implementación grupal de la funcionalidad de **Comunidades** en el sistema EDDMail, correspondiente a la Fase 2 del proyecto. Esta implementación fue desarrollada como parte del trabajo colaborativo requerido para demostrar el uso de estructuras de datos jerárquicas en un entorno de desarrollo en equipo.

La funcionalidad de comunidades permite a los usuarios crear grupos de comunicación organizados jerárquicamente mediante un árbol binario de búsqueda (BST), facilitando la colaboración y el intercambio de mensajes entre múltiples usuarios de manera eficiente.

## Objetivos de la Implementación Grupal

### Objetivo General
Implementar un sistema de comunidades funcional utilizando estructuras de datos jerárquicas, específicamente un árbol binario de búsqueda (BST), que permita la creación, gestión y navegación eficiente de comunidades en el sistema EDDMail.

### Objetivos Específicos
- Diseñar e implementar un árbol BST para el almacenamiento y organización de comunidades
- Desarrollar interfaz gráfica para la gestión de comunidades (creación, visualización)
- Implementar funcionalidad de publicación y visualización de mensajes en comunidades
- Crear sistema de reportes que visualice la estructura BST de comunidades
- Integrar la funcionalidad de comunidades con el sistema principal EDDMail

## Funcionalidad Asignada al Grupo

### Descripción de la Funcionalidad
**Sistema de Comunidades con Árbol BST**

La funcionalidad asignada consiste en implementar un sistema completo de comunidades que permite:

1. **Gestión de Comunidades:**
   - Creación de nuevas comunidades por el administrador
   - Almacenamiento en estructura BST ordenada por nombre
   - Búsqueda eficiente de comunidades existentes

2. **Sistema de Mensajes:**
   - Publicación de mensajes por usuarios en comunidades existentes
   - Visualización de mensajes publicados en cada comunidad
   - Validación de usuarios y comunidades antes de publicar

3. **Reportes y Visualización:**
   - Generación de reportes de la estructura BST
   - Creación de archivos Graphviz (.dot y .png)
   - Visualización gráfica de la organización de comunidades

### Estructuras de Datos Utilizadas
- **Árbol BST:** Para organización principal de comunidades
- **Lista Simple:** Para mensajes dentro de cada comunidad
- **Lista Simple:** Para usuarios miembros de cada comunidad

## Diseño e Implementación

### Arquitectura del Sistema

#### Componentes Principales
1. **CommunityTypes.pas:** Definición de tipos de datos para comunidades
2. **BST.pas:** Implementación del árbol binario de búsqueda
3. **CommunityManager.pas:** Lógica de gestión de comunidades
4. **CommunityDialog.pas:** Interfaz para crear comunidades
5. **PublishMessageDialog.pas:** Interfaz para publicar mensajes
6. **CommunityMessagesDialog.pas:** Interfaz para ver mensajes
7. **ReportGenerator.pas:** Generación de reportes BST

#### Estructura de Datos BST
```pascal
PBSTNode = ^TBSTNode;
TBSTNode = record
  community: TCommunity;
  left: PBSTNode;
  right: PBSTNode;
end;

TBST = class
private
  root: PBSTNode;
public
  procedure Insert(const community: TCommunity);
  function Search(const communityName: string): PBSTNode;
  procedure InOrder(var communities: TCommunityArray);
end;
```

#### Estructura de Comunidad
```pascal
TCommunity = record
  Id: Integer;
  Name: String;
  CreationDate: String;
  MessageCount: Integer;
  Users: TCommunityUserList;
  Messages: TCommunityMessageList;
end;
```

### Interfaces Implementadas

#### 1. Diálogo de Creación de Comunidades
- **Entrada:** Nombre de la comunidad
- **Validación:** Verificar que el nombre no exista
- **Resultado:** Inserción en BST y confirmación

#### 2. Diálogo de Publicación de Mensajes
- **Entrada:** Nombre de comunidad y mensaje
- **Validación:** Verificar existencia de comunidad
- **Resultado:** Agregar mensaje a la lista de la comunidad

#### 3. Diálogo de Visualización de Mensajes
- **Entrada:** Selección de comunidad
- **Resultado:** Mostrar lista de mensajes con autores

### Fase 3: Pruebas y Refinamiento
1. **Pruebas Unitarias:**
   - Verificación de operaciones BST
   - Validación de creación de comunidades
   - Pruebas de publicación de mensajes

2. **Pruebas de Integración:**
   - Verificación de flujo completo
   - Pruebas de reportes
   - Validación de interfaces

3. **Refinamiento:**
   - Corrección de errores encontrados
   - Mejoras en interfaz de usuario
   - Optimización de rendimiento

## Resultados Obtenidos

### Funcionalidades Implementadas Exitosamente

1. **Sistema de Comunidades BST Completo**
   - Creación de comunidades por administrador
   - Almacenamiento eficiente en árbol BST
   - Búsqueda logarítmica por nombre

2. **Sistema de Mensajes Funcional**
   - Publicación de mensajes por usuarios
   - Validación de comunidades existentes
   - Visualización organizada de mensajes

3. **Generación de Reportes**
   - Reportes de estructura BST
   - Visualización con Graphviz
   - Archivos PNG generados automáticamente

4. **Integración Completa**
   - Menús integrados en sistema principal
   - Funcionamiento sin conflictos
   - Interfaz coherente con resto del sistema
