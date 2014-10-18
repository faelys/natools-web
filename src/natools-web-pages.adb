------------------------------------------------------------------------------
-- Copyright (c) 2014, Natacha Porté                                        --
--                                                                          --
-- Permission to use, copy, modify, and distribute this software for any    --
-- purpose with or without fee is hereby granted, provided that the above   --
-- copyright notice and this permission notice appear in all copies.        --
--                                                                          --
-- THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES --
-- WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF         --
-- MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR  --
-- ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES   --
-- WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN    --
-- ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF  --
-- OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.           --
------------------------------------------------------------------------------

with Natools.S_Expressions.Caches;
with Natools.S_Expressions.File_Readers;
with Natools.S_Expressions.Interpreter_Loop;
with Natools.S_Expressions.Lockable;
with Natools.S_Expressions.Atom_Ref_Constructors;
with Natools.Static_Maps.Web.Pages;

package body Natools.Web.Pages is

   type Context_Type
     (Site : access constant Sites.Site;
      Page : access constant Page_Data)
     is null record;

   procedure Append
     (Exchange : in out Exchanges.Exchange;
      Context : in Context_Type;
      Data : in S_Expressions.Atom);

   procedure Execute
     (Data : in out Page_Data;
      Context : in Meaningless_Type;
      Name : in S_Expressions.Atom;
      Arguments : in out S_Expressions.Lockable.Descriptor'Class);

   procedure Render
     (Exchange : in out Exchanges.Exchange;
      Context : in Context_Type;
      Name : in S_Expressions.Atom;
      Arguments : in out S_Expressions.Lockable.Descriptor'Class);

   procedure Render
     (Exchange : in out Exchanges.Exchange;
      Context : in Context_Type;
      Template : in S_Expressions.Caches.Cursor);


   ---------------------------
   -- Page Data Constructor --
   ---------------------------

   procedure Execute
     (Data : in out Page_Data;
      Context : in Meaningless_Type;
      Name : in S_Expressions.Atom;
      Arguments : in out S_Expressions.Lockable.Descriptor'Class)
   is
      pragma Unreferenced (Context);
      package Components renames Natools.Static_Maps.Web.Pages;
   begin
      case Components.To_Component (S_Expressions.To_String (Name)) is
         when Components.Error =>
            Log (Severities.Error, "Unknown page component """
              & S_Expressions.To_String (Name) & '"');

         when Components.Elements =>
            Containers.Set_Expressions (Data.Elements, Arguments);
      end case;
   end Execute;


   procedure Read_Page is new S_Expressions.Interpreter_Loop
     (Page_Data, Meaningless_Type, Execute);



   -------------------
   -- Page Renderer --
   -------------------

   procedure Append
     (Exchange : in out Exchanges.Exchange;
      Context : in Context_Type;
      Data : in S_Expressions.Atom)
   is
      pragma Unreferenced (Context);
   begin
      Exchanges.Append (Exchange, Data);
   end Append;


   procedure Render
     (Exchange : in out Exchanges.Exchange;
      Context : in Context_Type;
      Name : in S_Expressions.Atom;
      Arguments : in out S_Expressions.Lockable.Descriptor'Class)
   is
      use type S_Expressions.Events.Event;
      package Commands renames Natools.Static_Maps.Web.Pages;
   begin
      case Commands.To_Command (S_Expressions.To_String (Name)) is
         when Commands.Unknown_Command =>
            null;

         when Commands.Element =>
            if Arguments.Current_Event = S_Expressions.Events.Add_Atom then
               Render (Exchange, Context, Containers.Get_Expression
                 (Context.Page.Elements, Arguments.Current_Atom));
            end if;

         when Commands.Template =>
            if Arguments.Current_Event = S_Expressions.Events.Add_Atom then
               Render (Exchange, Context,
                 Sites.Template (Context.Site.all, Arguments.Current_Atom));
            end if;
      end case;
   end Render;


   procedure Render_Page is new S_Expressions.Interpreter_Loop
     (Exchanges.Exchange, Context_Type, Render, Append);


   procedure Render
     (Exchange : in out Exchanges.Exchange;
      Context : in Context_Type;
      Template : in S_Expressions.Caches.Cursor)
   is
      Destroyed_Template : S_Expressions.Caches.Cursor := Template;
   begin
      Render_Page (Destroyed_Template, Exchange, Context);
   end Render;



   ----------------------
   -- Public Interface --
   ----------------------

   function Create (File_Path, Web_Path : in S_Expressions.Atom)
     return Page_Ref
   is
      function Create_Page return Page_Data;

      function Create_Page return Page_Data is
         Reader : Natools.S_Expressions.File_Readers.S_Reader
           := Natools.S_Expressions.File_Readers.Reader
              (S_Expressions.To_String (File_Path));
      begin
         return Result : Page_Data
           := (File_Path =>
                  S_Expressions.Atom_Ref_Constructors.Create (File_Path),
               Web_Path =>
                  S_Expressions.Atom_Ref_Constructors.Create (Web_Path),
               Elements => <>)
         do
            Read_Page (Reader, Result, Meaningless_Value);
         end return;
      end Create_Page;
   begin
      return (Ref => Data_Refs.Create (Create_Page'Access));
   end Create;


   overriding procedure Respond
     (Object : in out Page_Ref;
      Exchange : in out Exchanges.Exchange;
      Parent : aliased in Sites.Site;
      Extra_Path : in S_Expressions.Atom)
   is
      Template_Name : constant S_Expressions.Atom
        := Sites.Default_Template (Parent);
      Template : S_Expressions.Caches.Cursor;
      Accessor : constant Data_Refs.Accessor := Object.Ref.Query;
   begin
      if Extra_Path'Length > 0 then
         return;
      end if;

      declare
         Cursor : constant Containers.Expression_Maps.Cursor
           := Accessor.Data.Elements.Find (Template_Name);
      begin
         if Containers.Expression_Maps.Has_Element (Cursor) then
            Template := Containers.Expression_Maps.Element (Cursor);
         else
            Template := Sites.Template (Parent, Template_Name);
         end if;
      end;

      Render (Exchange, (Parent'Access, Accessor.Data), Template);
   end Respond;

end Natools.Web.Pages;
