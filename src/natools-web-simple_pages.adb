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
with Natools.S_Expressions.Atom_Ref_Constructors;
with Natools.Static_Maps.Web.Simple_Pages;
with Natools.Web.Error_Pages;
with Natools.Web.Exchanges;

package body Natools.Web.Simple_Pages is

   procedure Append
     (Exchange : in out Sites.Exchange;
      Page : in Page_Data;
      Data : in S_Expressions.Atom);

   procedure Execute
     (Data : in out Page_Data;
      Context : in Meaningless_Type;
      Name : in S_Expressions.Atom;
      Arguments : in out S_Expressions.Lockable.Descriptor'Class);

   procedure Render
     (Exchange : in out Sites.Exchange;
      Page : in Page_Data;
      Name : in S_Expressions.Atom;
      Arguments : in out S_Expressions.Lockable.Descriptor'Class);


   procedure Read_Page is new S_Expressions.Interpreter_Loop
     (Page_Data, Meaningless_Type, Execute);

   procedure Render_Page is new S_Expressions.Interpreter_Loop
     (Sites.Exchange, Page_Data, Render, Append);


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
      package Components renames Natools.Static_Maps.Web.Simple_Pages;
   begin
      case Components.To_Component (S_Expressions.To_String (Name)) is
         when Components.Error =>
            Log (Severities.Error, "Unknown page component """
              & S_Expressions.To_String (Name) & '"');

         when Components.Elements =>
            Containers.Set_Expressions (Data.Elements, Arguments);

         when Components.Tags =>
            Tags.Append (Data.Tags, Arguments);
      end case;
   end Execute;



   -------------------
   -- Page Renderer --
   -------------------

   procedure Append
     (Exchange : in out Sites.Exchange;
      Page : in Page_Data;
      Data : in S_Expressions.Atom)
   is
      pragma Unreferenced (Page);
   begin
      Exchange.Append (Data);
   end Append;


   procedure Render
     (Exchange : in out Sites.Exchange;
      Page : in Page_Data;
      Name : in S_Expressions.Atom;
      Arguments : in out S_Expressions.Lockable.Descriptor'Class)
   is
      use type S_Expressions.Events.Event;
      package Commands renames Natools.Static_Maps.Web.Simple_Pages;
   begin
      case Commands.To_Command (S_Expressions.To_String (Name)) is
         when Commands.Unknown_Command =>
            null;

         when Commands.Element =>
            declare
               Template : S_Expressions.Caches.Cursor
                 := Exchange.Site.Get_Template
                    (Page.Elements,
                     Arguments,
                     Lookup_Template => False);
            begin
               Render_Page (Template, Exchange, Page);
            end;

         when Commands.Element_Or_Template =>
            declare
               Template : S_Expressions.Caches.Cursor
                 := Exchange.Site.Get_Template (Page.Elements, Arguments);
            begin
               Render_Page (Template, Exchange, Page);
            end;

         when Commands.My_Tags =>
            if Arguments.Current_Event = S_Expressions.Events.Add_Atom then
               declare
                  Prefix : constant S_Expressions.Atom
                    := Arguments.Current_Atom;
               begin
                  Arguments.Next;
                  Tags.Render
                    (Exchange,
                     Page.Tags,
                     Exchange.Site.Get_Tags,
                     Prefix,
                     Arguments);
               end;
            end if;

         when Commands.Tags =>
            Tags.Render
              (Exchange,
               Exchange.Site.Get_Tags,
               Arguments,
               Page.Tags);

         when Commands.Template =>
            declare
               Template : S_Expressions.Caches.Cursor
                 := Exchange.Site.Get_Template
                    (Page.Elements,
                     Arguments,
                     Lookup_Element => False);
            begin
               Render_Page (Template, Exchange, Page);
            end;
      end case;
   end Render;



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

   function Create
     (File_Path, Web_Path : in S_Expressions.Atom_Refs.Immutable_Reference)
     return Page_Ref
   is
      function Create_Page return Page_Data;

      function Create_Page return Page_Data is
         Reader : Natools.S_Expressions.File_Readers.S_Reader
           := Natools.S_Expressions.File_Readers.Reader
              (S_Expressions.To_String (File_Path.Query));
      begin
         return Result : Page_Data
           := (File_Path => File_Path,
               Web_Path => Web_Path,
               Tags => <>,
               Elements => <>)
         do
            Read_Page (Reader, Result, Meaningless_Value);
         end return;
      end Create_Page;
   begin
      return (Ref => Data_Refs.Create (Create_Page'Access));
   end Create;


   overriding procedure Render
     (Exchange : in out Sites.Exchange;
      Object : in Page_Ref;
      Expression : in out S_Expressions.Lockable.Descriptor'Class) is
   begin
      Render_Page
        (Expression,
         Exchange,
         Object.Ref.Query.Data.all);
   end Render;


   overriding procedure Respond
     (Object : in out Page_Ref;
      Exchange : in out Sites.Exchange;
      Extra_Path : in S_Expressions.Atom) is
   begin
      if Extra_Path'Length > 0 then
         return;
      end if;

      declare
         Accessor : constant Data_Refs.Accessor := Object.Ref.Query;
         Expression : S_Expressions.Caches.Cursor;
      begin
         Check_Method :
         declare
            use Exchanges;
            Allowed : Boolean;
         begin
            Error_Pages.Check_Method
              (Exchange,
               (GET, HEAD),
               Allowed);

            if not Allowed then
               return;
            end if;
         end Check_Method;

         Expression := Exchange.Site.Get_Template
           (Accessor.Data.Elements,
            Expression,
            Exchange.Site.Default_Template,
            Lookup_Element => True,
            Lookup_Template => True,
            Lookup_Name => True);

         Render_Page (Expression, Exchange, Accessor.Data.all);
      end;
   end Respond;



   -----------------------
   -- Page Constructors --
   -----------------------

   function Create (File : in S_Expressions.Atom)
     return Sites.Page_Loader'Class is
   begin
      return Loader'(File_Path
        => S_Expressions.Atom_Ref_Constructors.Create (File));
   end Create;


   overriding procedure Load
     (Object : in out Loader;
      Builder : in out Sites.Site_Builder;
      Path : in S_Expressions.Atom)
   is
      Page : constant Page_Ref := Create
        (Object.File_Path,
         S_Expressions.Atom_Ref_Constructors.Create (Path));
   begin
      Sites.Insert (Builder, Path, Page);
      Sites.Insert (Builder, Page.Get_Tags, Page);
   end Load;


   procedure Register_Loader (Site : in out Sites.Site) is
   begin
      Site.Register ("simple-page", Create'Access);
   end Register_Loader;

end Natools.Web.Simple_Pages;
