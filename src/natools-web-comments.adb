------------------------------------------------------------------------------
-- Copyright (c) 2015, Natacha Porté                                        --
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

------------------------------------------------------------------------------
-- Natools.Web.Comments provides an implementation of user-posted comments  --
-- like is commonly found on blogs.                                         --
------------------------------------------------------------------------------

with Ada.Containers.Indefinite_Ordered_Maps;
with Ada.Exceptions;
with Ada.Streams;
with Natools.S_Expressions.Atom_Buffers;
with Natools.S_Expressions.Atom_Ref_Constructors;
with Natools.S_Expressions.Caches;
with Natools.S_Expressions.Conditionals.Generic_Evaluate;
with Natools.S_Expressions.Conditionals.Strings;
with Natools.S_Expressions.Interpreter_Loop;
with Natools.S_Expressions.Parsers;
with Natools.S_Expressions.Printers.Pretty;
with Natools.S_Expressions.Templates.Dates;
with Natools.S_Expressions.Templates.Integers;
with Natools.Static_Maps.Web.Comments;
with Natools.Time_IO.RFC_3339;
with Natools.Time_Keys;
with Natools.Web.Backends;
with Natools.Web.Error_Pages;
with Natools.Web.Escapes;
with Natools.Web.Exchanges;
with Natools.Web.Fallback_Render;
with Natools.Web.Filters.Text_Blocks;
with Natools.Web.Is_Valid_URL;
with Natools.Web.List_Templates;
with Natools.Web.Render_Default;
with Natools.Web.Sites.Updates;

package body Natools.Web.Comments is

   package Static_Maps renames Natools.Static_Maps.Web.Comments;

   package Comment_Maps is new Ada.Containers.Indefinite_Ordered_Maps
     (S_Expressions.Atom, Comment_Data, S_Expressions."<");

   package String_Maps is new Ada.Containers.Indefinite_Ordered_Maps
     (String, String);


   type Comment_Builder is record
      Core : Comment_Data;
      Extra_Fields : String_Maps.Map;
      Has_Unknown_Field : Boolean := False;
   end record;

   type Post_Action is (Save_Comment, Force_Preview, Parent_Redirect);


   Invalid_Condition : exception;


   Preview_Button : constant String := "preview";
   Preview_Id_Ref : S_Expressions.Atom_Refs.Immutable_Reference;
   Submit_Button : constant String := "submit";


   procedure Append
     (Exchange : in out Sites.Exchange;
      Context : in Comment_List;
      Data : in S_Expressions.Atom);
      --  Append Data to Exchange, ignoring Context

   procedure Append
     (Exchange : in out Sites.Exchange;
      Context : in Comment_Ref;
      Data : in S_Expressions.Atom);
      --  Append Data to Exchange, ignoring Context

   function Create (Data : S_Expressions.Atom)
     return S_Expressions.Atom_Refs.Immutable_Reference
     renames S_Expressions.Atom_Ref_Constructors.Create;
      --  Atom expression constructor

   function Evaluate_Parametric
     (Builder : in Comment_Builder;
      Name : in S_Expressions.Atom;
      Arguments : in out S_Expressions.Lockable.Descriptor'Class)
     return Boolean;
      --  Evaluate a condition on a comment builder with arguments

   function Evaluate_Simple
     (Builder : in Comment_Builder;
      Name : in S_Expressions.Atom)
     return Boolean;
      --  Evaluate a condition on a comment builder without argument

   function Parse_Action
     (Builder : in Comment_Builder;
      Expression : in out S_Expressions.Lockable.Descriptor'Class;
      Default_Action : in Post_Action := Save_Comment)
     return Post_Action;
      --  Evaluate Expression to determine what action to take after POST req

   function Parse_Action
     (Builder : in Comment_Builder;
      Site : in Sites.Site;
      Filter_Name : in S_Expressions.Atom_Refs.Immutable_Reference;
      Default_Action : in Post_Action := Save_Comment)
     return Post_Action;
      --  High-level evaluation, that extracts the expression from Site

   procedure Parse_Action
     (Result : in out Post_Action;
      Builder : in Comment_Builder;
      Name : in S_Expressions.Atom;
      Arguments : in out S_Expressions.Lockable.Descriptor'Class);
      --  Evluate action with parameter

   procedure Parse_Action_Simple
     (Result : in out Post_Action;
      Builder : in Comment_Builder;
      Name : in S_Expressions.Atom);
      --  Evaluate parameterless action

   function Preview_Id return S_Expressions.Atom_Refs.Immutable_Reference;
      --  Return the comment id of temporary previews

   procedure Process_Form
     (Data : in out Comment_Builder;
      Exchange : in Sites.Exchange);
      --  Read form data in Exchange to fill Data

   procedure Render_Comment_Position
     (Exchange : in out Sites.Exchange;
      Context : in Comment_Ref;
      Name : in S_Expressions.Atom;
      Arguments : in out S_Expressions.Lockable.Descriptor'Class);

   procedure Render_List_Element
     (Exchange : in out Sites.Exchange;
      List : in Comment_List;
      Name : in S_Expressions.Atom;
      Arguments : in out S_Expressions.Lockable.Descriptor'Class);
      --  Render a command

   function String_Fallback_Parametric
     (Settings : in S_Expressions.Conditionals.Strings.Settings;
      Name : in S_Expressions.Atom;
      Arguments : in out S_Expressions.Lockable.Descriptor'Class)
     return Boolean;
      --  Raise Invalid_Condition

   function String_Fallback_Simple
     (Settings : in S_Expressions.Conditionals.Strings.Settings;
      Name : in S_Expressions.Atom)
     return Boolean;
      --  Raise Invalid_Condition

   procedure Update_Item
     (Comment : in out Comment_Data;
      Context : in Meaningless_Type;
      Name : in S_Expressions.Atom;
      Arguments : in out S_Expressions.Lockable.Descriptor'Class);
      --  Update comment with the given expression

   procedure Update_List
     (List : in out Comment_List;
      Context : in Meaningless_Type;
      Name : in S_Expressions.Atom;
      Arguments : in out S_Expressions.Lockable.Descriptor'Class);
      --  Update comment list with the given expression


   function Evaluate is new S_Expressions.Conditionals.Generic_Evaluate
     (Comment_Builder, Evaluate_Parametric, Evaluate_Simple);

   procedure Parse is new S_Expressions.Interpreter_Loop
     (Post_Action, Comment_Builder, Parse_Action, Parse_Action_Simple);

   procedure Render is new S_Expressions.Interpreter_Loop
     (Sites.Exchange, Comment_List, Render_List_Element, Append);

   procedure Render is new S_Expressions.Interpreter_Loop
     (Sites.Exchange, Comment_Ref, Render_Comment_Position, Append);

   procedure Render_List is new List_Templates.Render (Comment_Iterators);

   procedure Update is new S_Expressions.Interpreter_Loop
     (Comment_Data, Meaningless_Type, Update_Item);

   procedure Update is new S_Expressions.Interpreter_Loop
     (Comment_List, Meaningless_Type, Update_List);



   ---------------
   -- Renderers --
   ---------------

   procedure Append
     (Exchange : in out Sites.Exchange;
      Context : in Comment_List;
      Data : in S_Expressions.Atom)
   is
      pragma Unreferenced (Context);
   begin
      Exchange.Append (Data);
   end Append;


   procedure Append
     (Exchange : in out Sites.Exchange;
      Context : in Comment_Ref;
      Data : in S_Expressions.Atom)
   is
      pragma Unreferenced (Context);
   begin
      Exchange.Append (Data);
   end Append;


   procedure Render_Comment_Position
     (Exchange : in out Sites.Exchange;
      Context : in Comment_Ref;
      Name : in S_Expressions.Atom;
      Arguments : in out S_Expressions.Lockable.Descriptor'Class)
   is
      use type Tags.Visible_Access;
      use Static_Maps.Item.Command;

      procedure Re_Enter
        (Exchange : in out Sites.Exchange;
         Expression : in out S_Expressions.Lockable.Descriptor'Class);

      procedure Render_Ref
        (Ref : in S_Expressions.Atom_Refs.Immutable_Reference);

      procedure Re_Enter
        (Exchange : in out Sites.Exchange;
         Expression : in out S_Expressions.Lockable.Descriptor'Class) is
      begin
         Render (Expression, Exchange, Context);
      end Re_Enter;

      procedure Render_Ref
        (Ref : in S_Expressions.Atom_Refs.Immutable_Reference) is
      begin
         if not Ref.Is_Empty then
            Exchange.Append (Ref.Query);
         elsif Arguments.Current_Event
           in S_Expressions.Events.Add_Atom | S_Expressions.Events.Open_List
         then
            Render (Arguments, Exchange, Context);
         end if;
      end Render_Ref;

      Accessor : constant Comment_Array_Refs.Accessor := Context.List.Query;
      Comment : Comment_Data renames Accessor.Data.Data (Context.Position);
   begin
      pragma Assert (Comment.Preprocessed);

      case Static_Maps.To_Item_Command (S_Expressions.To_String (Name)) is
         when Unknown =>
            Fallback_Render
              (Exchange, Name, Arguments, "comment", Re_Enter'Access);

         when Date =>
            S_Expressions.Templates.Dates.Render
              (Exchange, Arguments, Comment.Date);

         when Id =>
            Exchange.Append (Comment.Id.Query);

         when If_Link =>
            if not Comment.Link.Is_Empty then
               Render (Arguments, Exchange, Context);
            end if;

         when If_No_Link =>
            if Comment.Link.Is_Empty then
               Render (Arguments, Exchange, Context);
            end if;

         when If_No_Mail =>
            if Comment.Mail.Is_Empty then
               Render (Arguments, Exchange, Context);
            end if;

         when If_No_Name =>
            if Comment.Name.Is_Empty then
               Render (Arguments, Exchange, Context);
            end if;

         when If_Mail =>
            if not Comment.Mail.Is_Empty then
               Render (Arguments, Exchange, Context);
            end if;

         when If_Name =>
            if not Comment.Name.Is_Empty then
               Render (Arguments, Exchange, Context);
            end if;

         when Static_Maps.Item.Command.Name =>
            Render_Ref (Comment.Name);

         when Mail =>
            Render_Ref (Comment.Mail);

         when Link =>
            Render_Ref (Comment.Link);

         when Parent =>
            if Accessor.Parent /= null then
               Tags.Render (Exchange, Accessor.Parent.all, Arguments);
            end if;

         when Text =>
            Render_Ref (Comment.Text);
      end case;
   end Render_Comment_Position;


   procedure Render_List_Element
     (Exchange : in out Sites.Exchange;
      List : in Comment_List;
      Name : in S_Expressions.Atom;
      Arguments : in out S_Expressions.Lockable.Descriptor'Class)
   is
      use Static_Maps.List.Command;
   begin
      case Static_Maps.To_List_Command (S_Expressions.To_String (Name)) is
         when Unknown =>
            Log (Severities.Error, "Unknown comment list command """
              & S_Expressions.To_String (Name) & '"');

         when Static_Maps.List.Command.List =>
            Render_List
              (Exchange,
               Comment_Range'(List => List.Comments),
               List_Templates.Read_Parameters (Arguments));

         when Preview =>
            if Exchange.Parameter (Preview_Button) = ""
              and then Exchange.Parameter (Submit_Button) = ""
            then
               return;
            end if;

            declare
               Ref : constant Comment_Ref
                 := (List => Comment_Array_Refs.Create (new Comment_Container'
                       (Size => 1,
                        Data => (1 => <>),
                        Parent => List.Comments.Query.Parent)),
                     Position => 1);
               Builder : Comment_Builder;
            begin
               Builder.Core.Date := Ada.Calendar.Clock;
               Builder.Core.Id := Preview_Id;
               Process_Form (Builder, Exchange);
               Preprocess (Builder.Core);
               Ref.List.Update.Data (1) := Builder.Core;
               Render (Arguments, Exchange, Ref);
            end;

         when Size =>
            S_Expressions.Templates.Integers.Render
              (Exchange,
               Arguments,
               Integer (List.Comments.Query.Data.Data'Length));
      end case;
   end Render_List_Element;



   ------------------
   -- Interpreters --
   ------------------

   procedure Update_Item
     (Comment : in out Comment_Data;
      Context : in Meaningless_Type;
      Name : in S_Expressions.Atom;
      Arguments : in out S_Expressions.Lockable.Descriptor'Class)
   is
      pragma Unreferenced (Context);
      use Static_Maps.Item.Element;
      use type S_Expressions.Events.Event;
   begin
      pragma Assert (not Comment.Preprocessed);

      if Arguments.Current_Event /= S_Expressions.Events.Add_Atom then
         return;
      end if;

      case Static_Maps.To_Item_Element (S_Expressions.To_String (Name)) is
         when Unknown =>
            Log (Severities.Error,
              "Unknown comment element """
               & S_Expressions.To_String (Name) & '"');

         when Date =>
            declare
               Image : constant String
                 := S_Expressions.To_String (Arguments.Current_Atom);
            begin
               Comment.Date := Time_IO.RFC_3339.Value (Image);
            exception
               when others =>
                  Log (Severities.Error, "Invalid date """
                    & Image & """ for comment "
                    & S_Expressions.To_String (Comment.Id.Query));
            end;

         when Static_Maps.Item.Element.Name =>
            Comment.Name := Create (Arguments.Current_Atom);

         when Mail =>
            Comment.Mail := Create (Arguments.Current_Atom);

         when Link =>
            Comment.Link := Create (Arguments.Current_Atom);

         when Text =>
            Comment.Text := Create (Arguments.Current_Atom);
      end case;
   end Update_Item;


   procedure Update_List
     (List : in out Comment_List;
      Context : in Meaningless_Type;
      Name : in S_Expressions.Atom;
      Arguments : in out S_Expressions.Lockable.Descriptor'Class)
   is
      pragma Unreferenced (Context);
      use Static_Maps.List.Element;
      use type S_Expressions.Events.Event;
      Event : S_Expressions.Events.Event;
   begin
      if Arguments.Current_Event /= S_Expressions.Events.Add_Atom then
         return;
      end if;

      case Static_Maps.To_List_Element (S_Expressions.To_String (Name)) is
         when Unknown =>
            null;

         when Backend =>
            List.Backend_Name := Create (Arguments.Current_Atom);
            Arguments.Next (Event);

            if Event = S_Expressions.Events.Add_Atom then
               List.Backend_Path := Create (Arguments.Current_Atom);
            end if;

         when Post_Filter =>
            List.Post_Filter := Create (Arguments.Current_Atom);

         when Static_Maps.List.Element.Tags =>
            declare
               List_Builder : Containers.Unsafe_Atom_Lists.List;
            begin
               Containers.Append_Atoms (List_Builder, Arguments);
               List.Tags := Containers.Create (List_Builder);
            end;
      end case;
   end Update_List;



   --------------------------------
   -- Conditionals and Filtering --
   --------------------------------

   function Evaluate_Parametric
     (Builder : in Comment_Builder;
      Name : in S_Expressions.Atom;
      Arguments : in out S_Expressions.Lockable.Descriptor'Class)
     return Boolean
   is
      function Dereference
        (Ref : in S_Expressions.Atom_Refs.Immutable_Reference)
        return String;

      function String_Evaluate
        (Ref : in S_Expressions.Atom_Refs.Immutable_Reference;
         Arg : in out S_Expressions.Lockable.Descriptor'Class)
        return Boolean;

      function Dereference
        (Ref : in S_Expressions.Atom_Refs.Immutable_Reference)
        return String is
      begin
         if Ref.Is_Empty then
            return "";
         else
            return S_Expressions.To_String (Ref.Query);
         end if;
      end Dereference;

      function String_Evaluate
        (Ref : in S_Expressions.Atom_Refs.Immutable_Reference;
         Arg : in out S_Expressions.Lockable.Descriptor'Class)
        return Boolean
      is
         Value : aliased constant String := Dereference (Ref);
         Context : constant S_Expressions.Conditionals.Strings.Context
           := (Data => Value'Access,
               Parametric_Fallback => String_Fallback_Parametric'Access,
               Simple_Fallback => String_Fallback_Simple'Access,
               Settings => <>);
      begin
         return S_Expressions.Conditionals.Strings.Evaluate (Context, Arg);
      end String_Evaluate;

      use Static_Maps.Item.Condition;
   begin
      case Static_Maps.To_Item_Condition (S_Expressions.To_String (Name)) is
         when Unknown =>
            raise Invalid_Condition with "Unknown parametric conditional """
              & S_Expressions.To_String (Name) & '"';

         when Has_Extra_Fields =>
            return not Builder.Extra_Fields.Is_Empty;

         when Has_Unknown_Field =>
            return Builder.Has_Unknown_Field;

         when Link =>
            return String_Evaluate (Builder.Core.Link, Arguments);

         when Mail =>
            return String_Evaluate (Builder.Core.Mail, Arguments);

         when Static_Maps.Item.Condition.Name =>
            return String_Evaluate (Builder.Core.Name, Arguments);

         when Text =>
            return String_Evaluate (Builder.Core.Text, Arguments);
      end case;
   end Evaluate_Parametric;


   function Evaluate_Simple
     (Builder : in Comment_Builder;
      Name : in S_Expressions.Atom)
     return Boolean
   is
      use Static_Maps.Item.Condition;
   begin
      case Static_Maps.To_Item_Condition (S_Expressions.To_String (Name)) is
         when Unknown =>
            raise Invalid_Condition with "Unknown simple conditional """
              & S_Expressions.To_String (Name) & '"';

         when Has_Extra_Fields =>
            return not Builder.Extra_Fields.Is_Empty;

         when Has_Unknown_Field =>
            return Builder.Has_Unknown_Field;

         when Link =>
            return not Builder.Core.Link.Is_Empty;

         when Mail =>
            return not Builder.Core.Mail.Is_Empty;

         when Static_Maps.Item.Condition.Name =>
            return not Builder.Core.Name.Is_Empty;

         when Text =>
            return not Builder.Core.Text.Is_Empty;
      end case;
   end Evaluate_Simple;


   function Parse_Action
     (Builder : in Comment_Builder;
      Expression : in out S_Expressions.Lockable.Descriptor'Class;
      Default_Action : in Post_Action := Save_Comment)
     return Post_Action
   is
      Result : Post_Action := Default_Action;
   begin
      Parse (Expression, Result, Builder);
      return Result;
   end Parse_Action;


   function Parse_Action
     (Builder : in Comment_Builder;
      Site : in Sites.Site;
      Filter_Name : in S_Expressions.Atom_Refs.Immutable_Reference;
      Default_Action : in Post_Action := Save_Comment)
     return Post_Action
   is
      Expression : S_Expressions.Caches.Cursor;
      Found : Boolean;
   begin
      if Filter_Name.Is_Empty then
         return Default_Action;
      end if;

      Site.Get_Template (Filter_Name.Query, Expression, Found);

      if not Found then
         return Default_Action;
      end if;

      return Parse_Action (Builder, Expression, Default_Action);
   end Parse_Action;


   procedure Parse_Action
     (Result : in out Post_Action;
      Builder : in Comment_Builder;
      Name : in S_Expressions.Atom;
      Arguments : in out S_Expressions.Lockable.Descriptor'Class)
   is
      use Static_Maps.Item.Post_Action;
      S_Name : constant String := S_Expressions.To_String (Name);
   begin
      if S_Name = "if"
        and then Arguments.Current_Event
           in S_Expressions.Events.Add_Atom | S_Expressions.Events.Open_List
      then
         begin
            if Evaluate (Builder, Arguments) then
               Arguments.Next;
               Parse (Arguments, Result, Builder);
            end if;
         exception
            when Ex : Invalid_Condition =>
               Log (Severities.Error, "Invalid comment condition: "
                 & Ada.Exceptions.Exception_Message (Ex));
         end;

         return;
      end if;

      case Static_Maps.To_Item_Action (S_Name) is
         when Unknown =>
            Log (Severities.Error, "Unknown comment action """ & S_Name & '"');

         when Force_Preview =>
            Result := Force_Preview;

         when Reject =>
            Result := Parent_Redirect;

         when Save =>
            Result := Save_Comment;
      end case;
   end Parse_Action;


   procedure Parse_Action_Simple
     (Result : in out Post_Action;
      Builder : in Comment_Builder;
      Name : in S_Expressions.Atom)
   is
      pragma Unreferenced (Builder);
      use Static_Maps.Item.Post_Action;
      S_Name : constant String := S_Expressions.To_String (Name);
   begin
      case Static_Maps.To_Item_Action (S_Name) is
         when Unknown =>
            Log (Severities.Error, "Unknown comment action """ & S_Name & '"');

         when Force_Preview =>
            Result := Force_Preview;

         when Reject =>
            Result := Parent_Redirect;

         when Save =>
            Result := Save_Comment;
      end case;
   end Parse_Action_Simple;


   function String_Fallback_Parametric
     (Settings : in S_Expressions.Conditionals.Strings.Settings;
      Name : in S_Expressions.Atom;
      Arguments : in out S_Expressions.Lockable.Descriptor'Class)
     return Boolean
   is
      pragma Unreferenced (Settings, Arguments);
   begin
      raise Invalid_Condition with "Unknown string parametric conditional """
        & S_Expressions.To_String (Name) & '"';
      return False;
   end String_Fallback_Parametric;


   function String_Fallback_Simple
     (Settings : in S_Expressions.Conditionals.Strings.Settings;
      Name : in S_Expressions.Atom)
     return Boolean
   is
      pragma Unreferenced (Settings);
   begin
      raise Invalid_Condition with "Unknown string simple conditional """
        & S_Expressions.To_String (Name) & '"';
      return False;
   end String_Fallback_Simple;



   ------------------------
   -- Comment Suprograms --
   ------------------------

   procedure Preprocess (Comment : in out Comment_Data) is
   begin
      if Comment.Preprocessed then
         return;
      end if;

      if not Comment.Name.Is_Empty then
         Comment.Name := Escapes.Escape (Comment.Name, Escapes.HTML_Attribute);
      end if;

      if not Comment.Mail.Is_Empty then
         Comment.Mail := Escapes.Escape (Comment.Mail, Escapes.HTML_Attribute);
      end if;

      if not Comment.Link.Is_Empty
        and then Is_Valid_URL (S_Expressions.To_String (Comment.Link.Query))
      then
         Comment.Link := Escapes.Escape (Comment.Link, Escapes.HTML_Attribute);
      else
         Comment.Link.Reset;
      end if;

      if not Comment.Text.Is_Empty then
         declare
            Buffer : S_Expressions.Atom_Buffers.Atom_Buffer;
            Filter : Filters.Text_Blocks.Filter;
         begin
            Filter.Apply (Buffer, Comment.Text.Query);
            Comment.Text := Create (Buffer.Data);
         end;
      end if;

      Comment.Preprocessed := True;
   end Preprocess;


   function Preview_Id return S_Expressions.Atom_Refs.Immutable_Reference is
   begin
      if Preview_Id_Ref.Is_Empty then
         Preview_Id_Ref := Create (S_Expressions.To_Atom ("preview"));
      end if;

      return Preview_Id_Ref;
   end Preview_Id;


   procedure Process_Form
     (Data : in out Comment_Builder;
      Exchange : in Sites.Exchange)
   is
      procedure Process (Field, Value : String);

      procedure Process (Field, Value : String) is
         use Static_Maps.Item.Form;
      begin
         case Static_Maps.To_Item_Form (Field) is
            when Unknown =>
               Data.Extra_Fields.Insert (Field, Value);

               if Field /= Submit_Button and then Field /= Preview_Button then
                  Data.Has_Unknown_Field := True;
               end if;

            when Name =>
               Data.Core.Name := Create (S_Expressions.To_Atom (Value));

            when Mail =>
               Data.Core.Mail := Create (S_Expressions.To_Atom (Value));

            when Link =>
               Data.Core.Link := Create (S_Expressions.To_Atom (Value));

            when Text =>
               Data.Core.Text := Create (S_Expressions.To_Atom (Value));
         end case;
      end Process;
   begin
      Exchange.Iterate_Parameters (Process'Access);
   end Process_Form;


   overriding procedure Render
     (Exchange : in out Sites.Exchange;
      Object : in Comment_Ref;
      Expression : in out S_Expressions.Lockable.Descriptor'Class) is
   begin
      Render (Expression, Exchange, Object);
   end Render;


   procedure Write
     (Comment : in Comment_Data;
      Output : in out S_Expressions.Printers.Printer'Class)
   is
      procedure Print (Key : in String; Value : in S_Expressions.Atom);
      procedure Print
        (Key : in String;
         Value : in S_Expressions.Atom_Refs.Immutable_Reference);

      procedure Print (Key : in String; Value : in S_Expressions.Atom) is
      begin
         Output.Open_List;
         Output.Append_Atom (S_Expressions.To_Atom (Key));
         Output.Append_Atom (Value);
         Output.Close_List;
      end Print;

      procedure Print
        (Key : in String;
         Value : in S_Expressions.Atom_Refs.Immutable_Reference) is
      begin
         if not Value.Is_Empty then
            Print (Key, Value.Query);
         end if;
      end Print;
   begin
      pragma Assert (not Comment.Preprocessed);

      Print ("date", S_Expressions.To_Atom
        (Time_IO.RFC_3339.Image (Comment.Date)));

      Print ("name", Comment.Name);
      Print ("mail", Comment.Mail);
      Print ("link", Comment.Link);
      Print ("text", Comment.Text);
   end Write;



   ------------------------------
   -- Comment List Subprograms --
   ------------------------------

   overriding procedure Finalize (Object : in out Comment_List) is
   begin
      if not Object.Comments.Is_Empty then
         Object.Comments.Update.Parent := null;
         Object.Comments.Reset;
      end if;
   end Finalize;


   procedure Load
     (Object : in out Comment_List;
      Builder : in out Sites.Site_Builder;
      Parent : in Tags.Visible_Access := null;
      Parent_Path : in S_Expressions.Atom_Refs.Immutable_Reference
        := S_Expressions.Atom_Refs.Null_Immutable_Reference) is
   begin
      if Object.Backend_Name.Is_Empty or else Object.Backend_Path.Is_Empty then
         return;
      end if;

      Object.Parent_Path := Parent_Path;

      declare
         function Create return Comment_Container;
         procedure Process (Name : in S_Expressions.Atom);

         Backend : constant Backends.Backend'Class
           := Sites.Get_Backend (Builder, Object.Backend_Name.Query);
         Directory : constant S_Expressions.Atom_Refs.Accessor
           := Object.Backend_Path.Query;
         Map : Comment_Maps.Map;

         function Create return Comment_Container is
            Cursor : Comment_Maps.Cursor := Map.First;
         begin
            return Result : Comment_Container
              (S_Expressions.Offset (Map.Length))
            do
               Result.Parent := Parent;
               for I in Result.Data'Range loop
                  Result.Data (I) := Comment_Maps.Element (Cursor);
                  Comment_Maps.Next (Cursor);
               end loop;
               pragma Assert (not Comment_Maps.Has_Element (Cursor));
            end return;
         end Create;

         procedure Process (Name : in S_Expressions.Atom) is
            Input_Stream : aliased Ada.Streams.Root_Stream_Type'Class
              := Backend.Read (Directory, Name);
            Reader : S_Expressions.Parsers.Stream_Parser (Input_Stream'Access);
            Comment : Comment_Data;
            Position : Comment_Maps.Cursor;
            Inserted : Boolean;
         begin
            Reader.Next;
            Update (Reader, Comment, Meaningless_Value);
            Comment.Id := Create (Name);
            Preprocess (Comment);

            Map.Insert (Name, Comment, Position, Inserted);

            if not Inserted then
               Log (Severities.Error, "Duplicate comment id """
                 & S_Expressions.To_String (Name) & '"');
            end if;
         end Process;
      begin
         Backend.Iterate (Directory, Process'Access);
         Object.Comments := Comment_Array_Refs.Create (Create'Access);
      end;

      if not Object.Tags.Is_Empty then
         declare
            Accessor : constant Comment_Array_Refs.Accessor
              := Object.Comments.Query;
         begin
            for I in Accessor.Data.Data'Range loop
               Sites.Insert
                 (Builder,
                  Tags.Create (Object.Tags.Query, Accessor.Data (I).Id),
                  Comment_Ref'(Object.Comments, I));
            end loop;
         end;
      end if;
   end Load;


   procedure Render
     (Exchange : in out Sites.Exchange;
      Object : in Comment_List;
      Expression : in out S_Expressions.Lockable.Descriptor'Class) is
   begin
      if not Object.Comments.Is_Empty then
         Render (Expression, Exchange, Object);
      end if;
   end Render;


   procedure Respond
     (List : in out Comment_List;
      Exchange : in out Sites.Exchange;
      Extra_Path : in S_Expressions.Atom)
   is
      Builder : Comment_Builder;
   begin
      if Extra_Path'Length > 0
        or else List.Backend_Name.Is_Empty
        or else List.Backend_Path.Is_Empty
        or else List.Parent_Path.Is_Empty
      then
         return;
      end if;

      Check_Method :
      declare
         Allowed : Boolean;
      begin
         Error_Pages.Check_Method (Exchange, Exchanges.POST, Allowed);

         if not Allowed then
            return;
         end if;
      end Check_Method;

      if Exchange.Parameter (Preview_Button) /= "" then
         if not Tags."=" (List.Comments.Query.Parent, null) then
            Render_Default (Exchange, List.Comments.Query.Parent.all);
         end if;
         return;
      elsif Exchange.Parameter (Submit_Button) = "" then
         return;
      end if;

      Builder.Core.Date := Ada.Calendar.Clock;
      Builder.Core.Id := Create (S_Expressions.To_Atom
        (Time_Keys.To_Key (Builder.Core.Date)));
      Process_Form (Builder, Exchange);

      case Parse_Action (Builder, Exchange.Site.all, List.Post_Filter) is
         when Force_Preview =>
            if not Tags."=" (List.Comments.Query.Parent, null) then
               Render_Default (Exchange, List.Comments.Query.Parent.all);
            end if;

         when Parent_Redirect =>
            Error_Pages.See_Other (Exchange, List.Parent_Path.Query);

         when Save_Comment =>
            Write_Comment :
            declare
               Backend : Backends.Backend'Class
                 := Exchange.Site.Get_Backend (List.Backend_Name.Query);
               Stream : aliased Ada.Streams.Root_Stream_Type'Class
                 := Backend.Create
                    (List.Backend_Path.Query,
                     Builder.Core.Id.Query);
               Printer : S_Expressions.Printers.Pretty.Stream_Printer
                 (Stream'Access);
            begin
               Exchange.Site.Set_Parameters (Printer);
               Write (Builder.Core, Printer);
            end Write_Comment;

            Error_Pages.See_Other (Exchange, List.Parent_Path.Query);
            Sites.Updates.Reload (Exchange.Site.all);
      end case;
   end Respond;


   procedure Set
     (List : out Comment_List;
      Expression : in out S_Expressions.Lockable.Descriptor'Class) is
   begin
      List := Empty_List;
      Update (Expression, List, Meaningless_Value);
   end Set;



   --------------------------
   -- Iterator Subprograms --
   --------------------------

   procedure Render
     (Exchange : in out Sites.Exchange;
      Position : in Cursor;
      Expression : in out S_Expressions.Lockable.Descriptor'Class) is
   begin
      Render (Expression, Exchange, Position.Value);
   end Render;


   overriding function First (Object : Comment_Range) return Cursor is
   begin
      return (Value => (Object.List, Object.List.Query.Data.Data'First));
   end First;


   overriding function Last (Object : Comment_Range) return Cursor is
   begin
      return (Value => (Object.List, Object.List.Query.Data.Data'Last));
   end Last;


   overriding function Next (Object : Comment_Range; Position : Cursor)
     return Cursor
   is
      pragma Unreferenced (Object);
      use type S_Expressions.Offset;
   begin
      return (Value => (Position.Value.List, Position.Value.Position + 1));
   end Next;


   overriding function Previous (Object : Comment_Range; Position : Cursor)
     return Cursor
   is
      pragma Unreferenced (Object);
      use type S_Expressions.Offset;
   begin
      return (Value => (Position.Value.List, Position.Value.Position - 1));
   end Previous;

end Natools.Web.Comments;
