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
with Ada.Streams;
with Natools.S_Expressions.Atom_Ref_Constructors;
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
with Natools.Web.List_Templates;
with Natools.Web.Render_Default;
with Natools.Web.Sites.Updates;

package body Natools.Web.Comments is

   package Static_Maps renames Natools.Static_Maps.Web.Comments;

   package Comment_Maps is new Ada.Containers.Indefinite_Ordered_Maps
     (S_Expressions.Atom, Comment_Data, S_Expressions."<");

   package String_Maps is new Ada.Containers.Indefinite_Ordered_Maps
     (String, String);


   type Comment_Metadata is record
      Extra_Fields : String_Maps.Map;
   end record;


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

   function Preview_Id return S_Expressions.Atom_Refs.Immutable_Reference;
      --  Return the comment id of temporary previews

   procedure Process_Form
     (Comment : in out Comment_Data;
      Meta : in out Comment_Metadata;
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
      Accessor : constant Comment_Array_Refs.Accessor := Context.List.Query;
      Comment : Comment_Data renames Accessor.Data.Data (Context.Position);
   begin
      case Static_Maps.To_Item_Command (S_Expressions.To_String (Name)) is
         when Unknown =>
            Log (Severities.Error, "Unknown comment command """
              & S_Expressions.To_String (Name) & '"');

         when Date =>
            S_Expressions.Templates.Dates.Render
              (Exchange, Arguments, Comment.Date);

         when Static_Maps.Item.Command.Name =>
            Escapes.Write
              (Exchange, Comment.Name.Query, Escapes.HTML_Attribute);

         when Mail =>
            Escapes.Write
              (Exchange, Comment.Mail.Query, Escapes.HTML_Attribute);

         when Link =>
            Escapes.Write
              (Exchange, Comment.Link.Query, Escapes.HTML_Attribute);

         when Parent =>
            if Accessor.Parent /= null then
               Tags.Render (Exchange, Accessor.Parent.all, Arguments);
            end if;

         when Text =>
            Escapes.Write
              (Exchange, Comment.Text.Query, Escapes.HTML_Attribute);
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
            if Exchange.Parameter (Preview_Button) = "" then
               return;
            end if;

            declare
               Ref : constant Comment_Ref
                 := (List => Comment_Array_Refs.Create (new Comment_Container'
                       (Size => 1,
                        Data => (1 => (Date => Ada.Calendar.Clock,
                                       Id => Preview_Id,
                                       others => <>)),
                        Parent => List.Comments.Query.Parent)),
                     Position => 1);
               Meta : Comment_Metadata;
            begin
               Process_Form (Ref.List.Query.Data (1), Meta, Exchange);
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

         when Static_Maps.List.Element.Tags =>
            declare
               List_Builder : Containers.Unsafe_Atom_Lists.List;
            begin
               Containers.Append_Atoms (List_Builder, Arguments);
               List.Tags := Containers.Create (List_Builder);
            end;
      end case;
   end Update_List;



   ------------------------
   -- Comment Suprograms --
   ------------------------

   function Preview_Id return S_Expressions.Atom_Refs.Immutable_Reference is
   begin
      if Preview_Id_Ref.Is_Empty then
         Preview_Id_Ref := Create (S_Expressions.To_Atom ("preview"));
      end if;

      return Preview_Id_Ref;
   end Preview_Id;


   procedure Process_Form
     (Comment : in out Comment_Data;
      Meta : in out Comment_Metadata;
      Exchange : in Sites.Exchange)
   is
      procedure Process (Field, Value : String);

      procedure Process (Field, Value : String) is
         use Static_Maps.Item.Form;
      begin
         case Static_Maps.To_Item_Form (Field) is
            when Unknown =>
               Meta.Extra_Fields.Insert (Field, Value);

            when Name =>
               Comment.Name := Create (S_Expressions.To_Atom (Value));

            when Mail =>
               Comment.Mail := Create (S_Expressions.To_Atom (Value));

            when Link =>
               Comment.Link := Create (S_Expressions.To_Atom (Value));

            when Text =>
               Comment.Text := Create (S_Expressions.To_Atom (Value));
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

      procedure Print (Key : in String; Value : in S_Expressions.Atom) is
      begin
         Output.Open_List;
         Output.Append_Atom (S_Expressions.To_Atom (Key));
         Output.Append_Atom (Value);
         Output.Close_List;
      end Print;
   begin
      Print ("date", S_Expressions.To_Atom
        (Time_IO.RFC_3339.Image (Comment.Date)));

      Print ("name", Comment.Name.Query);
      Print ("mail", Comment.Mail.Query);
      Print ("link", Comment.Link.Query);
      Print ("text", Comment.Text.Query);
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
      New_Comment : Comment_Data;
      Meta : Comment_Metadata;
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

      New_Comment.Date := Ada.Calendar.Clock;
      New_Comment.Id := Create (S_Expressions.To_Atom
                          (Time_Keys.To_Key (New_Comment.Date)));
      Process_Form (New_Comment, Meta, Exchange);

      Write_Comment :
      declare
         Backend : Backends.Backend'Class
           := Exchange.Site.Get_Backend (List.Backend_Name.Query);
         Stream : aliased Ada.Streams.Root_Stream_Type'Class
           := Backend.Create (List.Backend_Path.Query, New_Comment.Id.Query);
         Printer : S_Expressions.Printers.Pretty.Stream_Printer
           (Stream'Access);
      begin
         Exchange.Site.Set_Parameters (Printer);
         Write (New_Comment, Printer);
      end Write_Comment;

      Error_Pages.See_Other (Exchange, List.Parent_Path.Query);
      Sites.Updates.Reload (Exchange.Site.all);
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
