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

with Natools.S_Expressions.File_Readers;
with Natools.S_Expressions.Interpreter_Loop;
with Natools.S_Expressions.Lockable;
with Natools.S_Expressions.Atom_Ref_Constructors;
with Natools.Static_Maps.Web.Pages;
with Natools.Web.Error_Pages;

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

   procedure Sub_Render
     (Exchange : in out Exchanges.Exchange;
      Context : in Context_Type;
      Expression : in out S_Expressions.Lockable.Descriptor'Class;
      Lookup_Element : in Boolean := False;
      Lookup_Template : in Boolean := False);

   procedure Sub_Render
     (Exchange : in out Exchanges.Exchange;
      Context : in Context_Type;
      Name : in S_Expressions.Atom;
      Fallback : in out S_Expressions.Lockable.Descriptor'Class;
      Lookup_Element : in Boolean;
      Lookup_Template : in Boolean)
     with Pre => Lookup_Element or Lookup_Template;


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
            Sub_Render (Exchange, Context, Arguments, Lookup_Element => True);

         when Commands.Template =>
            Sub_Render (Exchange, Context, Arguments, Lookup_Template => True);
      end case;
   end Render;


   procedure Render_Page is new S_Expressions.Interpreter_Loop
     (Exchanges.Exchange, Context_Type, Render, Append);


   procedure Sub_Render
     (Exchange : in out Exchanges.Exchange;
      Context : in Context_Type;
      Expression : in out S_Expressions.Lockable.Descriptor'Class;
      Lookup_Element : in Boolean := False;
      Lookup_Template : in Boolean := False)
   is
      use type S_Expressions.Events.Event;
   begin
      if (Lookup_Element or Lookup_Template)
        and then Expression.Current_Event = S_Expressions.Events.Add_Atom
      then
         declare
            Name : constant S_Expressions.Atom := Expression.Current_Atom;
         begin
            Expression.Next;
            Sub_Render
              (Exchange, Context,
               Name, Expression,
               Lookup_Element, Lookup_Template);
         end;
      else
         Render_Page (Expression, Exchange, Context);
      end if;
   end Sub_Render;


   procedure Sub_Render
     (Exchange : in out Exchanges.Exchange;
      Context : in Context_Type;
      Name : in S_Expressions.Atom;
      Fallback : in out S_Expressions.Lockable.Descriptor'Class;
      Lookup_Element : in Boolean;
      Lookup_Template : in Boolean)
   is
      Template : S_Expressions.Caches.Cursor;
      Found : Boolean;
   begin
      if Lookup_Element then
         Context.Page.Get_Element (Name, Template, Found);

         if Found then
            Render_Page (Template, Exchange, Context);
            return;
         end if;
      end if;

      if Lookup_Template then
         Sites.Get_Template (Context.Site.all, Name, Template, Found);

         if Found then
            Render_Page (Template, Exchange, Context);
            return;
         end if;
      end if;

      case Fallback.Current_Event is
         when S_Expressions.Events.Close_List
           | S_Expressions.Events.End_Of_Input
           | S_Expressions.Events.Error
         =>
            Log (Severities.Error,
              "Page expression """
              & S_Expressions.To_String (Name)
              & """ not found");
            return;

         when S_Expressions.Events.Open_List
           | S_Expressions.Events.Add_Atom
         =>
            Render_Page (Fallback, Exchange, Context);
      end case;
   end Sub_Render;



   -------------------------
   -- Page_Data Interface --
   -------------------------

   procedure Get_Element
     (Data : in Page_Data;
      Name : in S_Expressions.Atom;
      Element : out S_Expressions.Caches.Cursor;
      Found : out Boolean)
   is
      Cursor : constant Containers.Expression_Maps.Cursor
        := Data.Elements.Find (Name);
   begin
      Found := Containers.Expression_Maps.Has_Element (Cursor);

      if Found then
         Element := Containers.Expression_Maps.Element (Cursor);
      end if;
   end Get_Element;



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
      Parent : not null access constant Sites.Site;
      Extra_Path : in S_Expressions.Atom) is
   begin
      if Extra_Path'Length > 0 then
         return;
      end if;

      Check_Method :
      declare
         use Exchanges;
         Allowed : Boolean;
      begin
         Error_Pages.Check_Method (Exchange, Parent, (GET, HEAD), Allowed);

         if not Allowed then
            return;
         end if;
      end Check_Method;

      declare
         Accessor : constant Data_Refs.Accessor := Object.Ref.Query;
         Null_Expression : S_Expressions.Caches.Cursor;
      begin
         Sub_Render
           (Exchange,
            (Parent, Accessor.Data),
            Sites.Default_Template (Parent.all),
            Null_Expression,
            Lookup_Element => True,
            Lookup_Template => True);
      end;
   end Respond;

end Natools.Web.Pages;
