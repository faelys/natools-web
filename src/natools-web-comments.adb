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

with Ada.Containers.Indefinite_Ordered_Sets;
with Ada.Streams;
with Natools.S_Expressions.Atom_Ref_Constructors;
with Natools.S_Expressions.Interpreter_Loop;
with Natools.S_Expressions.Parsers;
with Natools.S_Expressions.Templates.Dates;
with Natools.S_Expressions.Templates.Integers;
with Natools.Static_Maps.Web.Comments;
with Natools.Time_IO.RFC_3339;
with Natools.Web.Backends;
with Natools.Web.List_Templates;

package body Natools.Web.Comments is

   package Static_Maps renames Natools.Static_Maps.Web.Comments;

   package Comment_Sets is new Ada.Containers.Indefinite_Ordered_Sets
     (Comment_Data);


   procedure Append
     (Exchange : in out Sites.Exchange;
      Context : in Comment_Data;
      Data : in S_Expressions.Atom);
      --  Append Data to Exchange, ignoring Context

   procedure Append
     (Exchange : in out Sites.Exchange;
      Context : in Comment_List;
      Data : in S_Expressions.Atom);
      --  Append Data to Exchange, ignoring Context

   function Create (Data : S_Expressions.Atom)
     return S_Expressions.Atom_Refs.Immutable_Reference
     renames S_Expressions.Atom_Ref_Constructors.Create;
      --  Atom expression constructor

   procedure HTML_Escape
     (Output : in out Sites.Exchange;
      Data : in S_Expressions.Atom);
      --  Escape '<', '>' and '"', but not '&' since entities are sent by
      --  some clients.

   procedure Render_Comment_Element
     (Exchange : in out Sites.Exchange;
      Comment : in Comment_Data;
      Name : in S_Expressions.Atom;
      Arguments : in out S_Expressions.Lockable.Descriptor'Class);
      --  Render a command

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
     (Sites.Exchange, Comment_Data, Render_Comment_Element, Append);

   procedure Render is new S_Expressions.Interpreter_Loop
     (Sites.Exchange, Comment_List, Render_List_Element, Append);

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
      Context : in Comment_Data;
      Data : in S_Expressions.Atom)
   is
      pragma Unreferenced (Context);
   begin
      Exchange.Append (Data);
   end Append;


   procedure Append
     (Exchange : in out Sites.Exchange;
      Context : in Comment_List;
      Data : in S_Expressions.Atom)
   is
      pragma Unreferenced (Context);
   begin
      Exchange.Append (Data);
   end Append;


   procedure HTML_Escape
     (Output : in out Sites.Exchange;
      Data : in S_Expressions.Atom)
   is
      use type S_Expressions.Octet;
      use type S_Expressions.Offset;

      Less_Than : constant S_Expressions.Octet := Character'Pos ('<');
      Greater_Than : constant S_Expressions.Octet := Character'Pos ('>');
      Quote : constant S_Expressions.Octet := Character'Pos ('"');

      subtype Octet_To_Escape is S_Expressions.Octet with Static_Predicate
        => Octet_To_Escape in Less_Than | Greater_Than | Quote;

      Last_Done : S_Expressions.Offset := Data'First - 1;
      I : S_Expressions.Offset := Data'First;
   begin
      Process_All :
      while I in Data'Range loop
         Search_Escape :
         while I in Data'Range and then Data (I) not in Octet_To_Escape loop
            I := I + 1;
         end loop Search_Escape;

         if Last_Done + 1 <= I - 1 then
            Output.Append (Data (Last_Done + 1 .. I - 1));
            Last_Done := I - 1;
         end if;

         Perform_Escape :
         while I in Data'Range loop
            case Data (I) is
               when Less_Than =>
                  Output.Append
                    ((Character'Pos ('&'), Character'Pos ('l'),
                      Character'Pos ('t'), Character'Pos (';')));

               when Greater_Than =>
                  Output.Append
                    ((Character'Pos ('&'), Character'Pos ('g'),
                      Character'Pos ('t'), Character'Pos (';')));

               when Quote =>
                  Output.Append
                    ((Character'Pos ('&'), Character'Pos ('q'),
                      Character'Pos ('u'), Character'Pos ('o'),
                      Character'Pos ('t'), Character'Pos (';')));

               when others =>
                  exit Perform_Escape;
            end case;

            Last_Done := I;
            I := I + 1;
         end loop Perform_Escape;
      end loop Process_All;
   end HTML_Escape;


   procedure Render_Comment_Element
     (Exchange : in out Sites.Exchange;
      Comment : in Comment_Data;
      Name : in S_Expressions.Atom;
      Arguments : in out S_Expressions.Lockable.Descriptor'Class)
   is
      use Static_Maps.Item.Command;
   begin
      case Static_Maps.To_Item_Command (S_Expressions.To_String (Name)) is
         when Unknown =>
            Log (Severities.Error, "Unknown comment list command """
              & S_Expressions.To_String (Name) & '"');

         when Date =>
            S_Expressions.Templates.Dates.Render
              (Exchange, Arguments, Comment.Date);

         when Static_Maps.Item.Command.Name =>
            HTML_Escape (Exchange, Comment.Name.Query);

         when Mail =>
            HTML_Escape (Exchange, Comment.Mail.Query);

         when Link =>
            HTML_Escape (Exchange, Comment.Link.Query);

         when Text =>
            HTML_Escape (Exchange, Comment.Text.Query);
      end case;
   end Render_Comment_Element;


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

         when Size =>
            S_Expressions.Templates.Integers.Render
              (Exchange,
               Arguments,
               Integer (List.Comments.Query.Data.all'Length));
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
      use Static_Maps.Item.Command;
      use type S_Expressions.Events.Event;
   begin
      if Arguments.Current_Event /= S_Expressions.Events.Add_Atom then
         return;
      end if;

      case Static_Maps.To_Item_Command (S_Expressions.To_String (Name)) is
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

         when Static_Maps.Item.Command.Name =>
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

   overriding procedure Render
     (Exchange : in out Sites.Exchange;
      Object : in Comment_Ref;
      Expression : in out S_Expressions.Lockable.Descriptor'Class) is
   begin
      Render
        (Expression,
         Exchange,
         Object.List.Query.Data.all (Object.Position));
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

   procedure Load
     (Object : in out Comment_List;
      Builder : in out Sites.Site_Builder) is
   begin
      if Object.Backend_Name.Is_Empty or else Object.Backend_Path.Is_Empty then
         return;
      end if;

      declare
         function Create return Comment_Array;
         procedure Process (Name : in S_Expressions.Atom);

         Backend : constant Backends.Backend'Class
           := Sites.Get_Backend (Builder, Object.Backend_Name.Query);
         Directory : constant S_Expressions.Atom_Refs.Accessor
           := Object.Backend_Path.Query;
         Set : Comment_Sets.Set;

         function Create return Comment_Array is
            Cursor : Comment_Sets.Cursor := Set.First;
         begin
            return Result : Comment_Array
              (1 .. S_Expressions.Offset (Set.Length))
            do
               for I in Result'Range loop
                  Result (I) := Comment_Sets.Element (Cursor);
                  Comment_Sets.Next (Cursor);
               end loop;
               pragma Assert (not Comment_Sets.Has_Element (Cursor));
            end return;
         end Create;

         procedure Process (Name : in S_Expressions.Atom) is
            Input_Stream : aliased Ada.Streams.Root_Stream_Type'Class
              := Backend.Read (Directory, Name);
            Reader : S_Expressions.Parsers.Stream_Parser (Input_Stream'Access);
            Comment : Comment_Data;
            Position : Comment_Sets.Cursor;
            Inserted : Boolean;
         begin
            Reader.Next;
            Update (Reader, Comment, Meaningless_Value);
            Comment.Id := Create (Name);

            Set.Insert (Comment, Position, Inserted);

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
            for I in Accessor.Data.all'Range loop
               Sites.Insert
                 (Builder,
                  Tags.Create (Object.Tags.Query, Accessor (I).Id),
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
      Render
        (Expression,
         Exchange,
         Position.Value.List.Query.Data.all (Position.Value.Position));
   end Render;


   overriding function First (Object : Comment_Range) return Cursor is
   begin
      return (Value => (Object.List, Object.List.Query.Data.all'First));
   end First;


   overriding function Last (Object : Comment_Range) return Cursor is
   begin
      return (Value => (Object.List, Object.List.Query.Data.all'Last));
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
