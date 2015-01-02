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

with Natools.S_Expressions.Atom_Ref_Constructors;
with Natools.S_Expressions.Caches;
with Natools.S_Expressions.File_Readers;
with Natools.S_Expressions.Interpreter_Loop;
with Natools.S_Expressions.Lockable;
with Natools.Static_Maps.Web.Tag_Pages;
with Natools.Web.Error_Pages;
with Natools.Web.Exchanges;
with Natools.Web.Tags;

package body Natools.Web.Tag_Pages is

   package Constructors renames Natools.S_Expressions.Atom_Ref_Constructors;
   package Commands renames Natools.Static_Maps.Web.Tag_Pages;

   type Rendering_Context is record
      Root_Tag : S_Expressions.Atom_Refs.Immutable_Reference;
      Current : Tags.Tag_Contents;
      Mode : Page_Mode;
      Elements : Containers.Expression_Maps.Constant_Map;
   end record;


   procedure Append
     (Exchange : in out Sites.Exchange;
      Context : in Rendering_Context;
      Data : in S_Expressions.Atom);

   procedure Load_Command
     (Target : in out Page;
      Context : in Meaningless_Type;
      Name : in S_Expressions.Atom;
      Arguments : in out S_Expressions.Lockable.Descriptor'Class);

   procedure Load_Simple
     (Target : in out Page;
      Context : in Meaningless_Type;
      Name : in S_Expressions.Atom);

   procedure Render_Command
     (Exchange : in out Sites.Exchange;
      Context : in Rendering_Context;
      Name : in S_Expressions.Atom;
      Arguments : in out S_Expressions.Lockable.Descriptor'Class);


   procedure Load is new S_Expressions.Interpreter_Loop
     (Page, Meaningless_Type, Load_Command, Load_Simple);

   procedure Render is new S_Expressions.Interpreter_Loop
     (Sites.Exchange, Rendering_Context, Render_Command, Append);



   -------------------------------
   -- Configuration Interpreter --
   -------------------------------

   procedure Load_Command
     (Target : in out Page;
      Context : in Meaningless_Type;
      Name : in S_Expressions.Atom;
      Arguments : in out S_Expressions.Lockable.Descriptor'Class)
   is
      pragma Unreferenced (Context);
      use type S_Expressions.Events.Event;
   begin
      case Commands.To_Component (S_Expressions.To_String (Name)) is
         when Commands.Unknown_Component =>
            Log (Severities.Error, "Unknown tag page component """
              & S_Expressions.To_String (Name) & '"');

         when Commands.Elements =>
            declare
               Elements : Containers.Expression_Maps.Constant_Map;
            begin
               Containers.Set_Expressions (Elements, Arguments);
               Target.Elements := (others => Elements);
            end;

         when Commands.Index_Elements =>
            declare
               Elements : Containers.Expression_Maps.Constant_Map;
            begin
               Containers.Set_Expressions (Elements, Arguments);
               Target.Elements (Slash_Index) := Elements;
               Target.Elements (Slashless_Index) := Elements;
            end;

         when Commands.No_Slash_Index =>
            Target.Elements (Slash_Index).Clear;

         when Commands.No_Slash_Redirect =>
            Target.Redirect (Slash_Index) := False;

         when Commands.No_Slashless_Index =>
            Target.Elements (Slashless_Index).Clear;

         when Commands.No_Slashless_Redirect =>
            Target.Redirect (Slashless_Index) := False;

         when Commands.Root =>
            if Arguments.Current_Event = S_Expressions.Events.Add_Atom then
               Target.Root_Tag
                 := Constructors.Create (Arguments.Current_Atom);
            end if;

         when Commands.Slash_Index_Elements =>
            Containers.Set_Expressions
              (Target.Elements (Slash_Index),
               Arguments);

         when Commands.Slash_Redirect =>
            Target.Redirect (Slash_Index) := True;

         when Commands.Slashless_Index_Elements =>
            Containers.Set_Expressions
              (Target.Elements (Slashless_Index),
               Arguments);

         when Commands.Slashless_Redirect =>
            Target.Redirect (Slashless_Index) := True;

         when Commands.Tag_Elements =>
            Containers.Set_Expressions
              (Target.Elements (Child),
               Arguments);
      end case;
   end Load_Command;


   procedure Load_Simple
     (Target : in out Page;
      Context : in Meaningless_Type;
      Name : in S_Expressions.Atom)
   is
      pragma Unreferenced (Context);
   begin
      case Commands.To_Component (S_Expressions.To_String (Name)) is
         when Commands.Unknown_Component =>
            Log (Severities.Error, "Unknown tag page component """
              & S_Expressions.To_String (Name) & '"');

         when Commands.Elements =>
            Target.Elements (Child).Clear;
            Target.Elements (Slash_Index).Clear;
            Target.Elements (Slashless_Index).Clear;

         when Commands.Index_Elements =>
            Target.Elements (Slash_Index).Clear;
            Target.Elements (Slashless_Index).Clear;

         when Commands.No_Slash_Index =>
            Target.Elements (Slash_Index).Clear;

         when Commands.No_Slash_Redirect =>
            Target.Redirect (Slash_Index) := False;

         when Commands.No_Slashless_Index =>
            Target.Elements (Slashless_Index).Clear;

         when Commands.No_Slashless_Redirect =>
            Target.Redirect (Slashless_Index) := False;

         when Commands.Root =>
            Target.Root_Tag.Reset;

         when Commands.Slash_Index_Elements =>
            Target.Elements (Slash_Index).Clear;

         when Commands.Slash_Redirect =>
            Target.Redirect (Slash_Index) := True;

         when Commands.Slashless_Index_Elements =>
            Target.Elements (Slashless_Index).Clear;

         when Commands.Slashless_Redirect =>
            Target.Redirect (Slashless_Index) := True;

         when Commands.Tag_Elements =>
            Target.Elements (Child).Clear;
      end case;
   end Load_Simple;



   --------------
   -- Renderer --
   --------------

   procedure Append
     (Exchange : in out Sites.Exchange;
      Context : in Rendering_Context;
      Data : in S_Expressions.Atom)
   is
      pragma Unreferenced (Context);
   begin
      Exchange.Append (Data);
   end Append;


   procedure Render_Command
     (Exchange : in out Sites.Exchange;
      Context : in Rendering_Context;
      Name : in S_Expressions.Atom;
      Arguments : in out S_Expressions.Lockable.Descriptor'Class)
   is
   begin
      case Commands.To_Command (S_Expressions.To_String (Name)) is
         when Commands.Unknown_Command =>
            Log (Severities.Error, "Unknown tag page command """
              & S_Expressions.To_String (Name) & '"');

         when Commands.Current_Tag =>
            Tags.Render (Exchange, Context.Current, Arguments);

         when Commands.Element =>
            declare
               Template : S_Expressions.Caches.Cursor
                 := Exchange.Site.Get_Template
                    (Context.Elements,
                     Arguments,
                     Lookup_Template => False);
            begin
               Render (Template, Exchange, Context);
            end;

         when Commands.Element_Or_Template =>
            declare
               Template : S_Expressions.Caches.Cursor
                 := Exchange.Site.Get_Template (Context.Elements, Arguments);
            begin
               Render (Template, Exchange, Context);
            end;

         when Commands.If_Index =>
            if Context.Mode in Slash_Index | Slashless_Index then
               Render (Arguments, Exchange, Context);
            end if;

         when Commands.If_Slash_Index =>
            if Context.Mode = Slash_Index then
               Render (Arguments, Exchange, Context);
            end if;

         when Commands.If_Slashless_Index =>
            if Context.Mode = Slashless_Index then
               Render (Arguments, Exchange, Context);
            end if;

         when Commands.If_Tag =>
            if Context.Mode = Child then
               Render (Arguments, Exchange, Context);
            end if;

         when Commands.Root_Tag =>
            Exchange.Append (Context.Root_Tag.Query);

         when Commands.Template =>
            declare
               Template : S_Expressions.Caches.Cursor
                 := Exchange.Site.Get_Template
                    (Context.Elements,
                     Arguments,
                     Lookup_Element => False);
            begin
               Render (Template, Exchange, Context);
            end;

      end case;
   end Render_Command;



   -------------------
   -- HTTP Response --
   -------------------

   overriding procedure Respond
     (Object : in out Page;
      Exchange : in out Sites.Exchange;
      Extra_Path : in S_Expressions.Atom)
   is
      pragma Unmodified (Object);
      use type S_Expressions.Atom;

      Context : Rendering_Context;
   begin
      if Object.Root_Tag.Is_Empty then
         return;
      end if;

      if Extra_Path'Length = 0 then
         Context.Mode := Slashless_Index;
         Context.Current := Tags.Get_Tag
           (Exchange.Site.Get_Tags, Object.Root_Tag.Query);
      elsif Extra_Path'Length = 1 then
         Context.Mode := Slash_Index;
         Context.Current := Tags.Get_Tag
           (Exchange.Site.Get_Tags, Object.Root_Tag.Query);
      else
         Context.Mode := Child;
         Context.Current := Tags.Get_Tag
           (Exchange.Site.Get_Tags,
            Object.Root_Tag.Query & Extra_Path);

         if Tags.Is_Empty (Context.Current) then
            return;
         end if;
      end if;

      if Object.Elements (Context.Mode).Is_Empty then
         return;
      end if;

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

      Context.Elements := Object.Elements (Context.Mode);
      Context.Root_Tag := Object.Root_Tag;

      declare
         Expression : S_Expressions.Caches.Cursor;
      begin
         Expression := Exchange.Site.Get_Template
           (Context.Elements,
            Expression,
            Exchange.Site.Default_Template,
            Lookup_Element => True,
            Lookup_Template => True,
            Lookup_Name => True);

         Render (Expression, Exchange, Context);
      end;
   end Respond;


   ----------------------
   -- Page Constructor --
   ----------------------

   function Create (File : in S_Expressions.Atom)
     return Sites.Page_Loader'Class is
   begin
      return Loader'(File => Constructors.Create (File));
   end Create;


   overriding procedure Load
     (Object : in out Loader;
      Builder : in out Sites.Site_Builder;
      Path : in S_Expressions.Atom)
   is
      File_Name : constant String
        := S_Expressions.To_String (Object.File.Query);
      New_Page : Page;
   begin
      declare
         Reader : S_Expressions.File_Readers.S_Reader
           := S_Expressions.File_Readers.Reader (File_Name);
      begin
         Load (Reader, New_Page, Meaningless_Value);
      end;

      if New_Page.Root_Tag.Is_Empty then
         Log (Severities.Error, "No root tag for tag page " & File_Name);
         return;
      end if;

      Sites.Insert (Builder, Path, New_Page);
   end Load;


   procedure Register_Loader (Site : in out Sites.Site) is
   begin
      Site.Register ("tag-page", Create'Access);
   end Register_Loader;

end Natools.Web.Tag_Pages;
