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
with Natools.S_Expressions.Interpreter_Loop;
with Natools.Static_Maps.Web.Tags;
with Natools.Web.List_Templates;

package body Natools.Web.Tags is

   use type S_Expressions.Atom;
   use type S_Expressions.Octet;
   use type S_Expressions.Offset;

   type Offset_Array is
     array (S_Expressions.Count range <>) of S_Expressions.Offset;

   type Processed_Name
     (Size, Separator_Count : S_Expressions.Count)
   is record
      Full : S_Expressions.Atom (1 .. Size);
      Separators : Offset_Array (1 .. Separator_Count);
   end record;

   type Tag_List_Context is record
      DB : Tag_DB;
      Parent_Tags : Tag_List;
   end record;


   Path_Separator : constant S_Expressions.Octet := Character'Pos ('/');


   function Is_Path_Prefix (Small, Large : S_Expressions.Atom) return Boolean
     is (Small'Length <= Large'Length
         and then Small = Large (Large'First .. Large'First + Small'Length - 1)
         and then (Small'Length = Large'Length
            or else Large (Large'First + Small'Length) = Path_Separator));
      --  Check whether Small is a parent path of Large


   procedure Add_Tags_To_List
     (List : in out Tag_List;
      Context : in Meaningless_Type;
      Key : in S_Expressions.Atom;
      Expression : in out S_Expressions.Lockable.Descriptor'Class);
      --  Parse Expression for tag names and add (tag, key) to List

   procedure Append
     (List : in out Tag_List;
      Item : in Tag_Description);
      --  Append a single tag to List.
      --  This is a simple O(n) copy, which leads to quadratic list
      --  construction. This is considered acceptable since lists are expected
      --  to be small and seldom (re)built.

   procedure Append
     (Exchange : in out Exchanges.Exchange;
      Tag : in Tag_Contents;
      Data : in S_Expressions.Atom);
      --  Append Data to Exchange, ignoring Tag

   procedure Append
     (Exchange : in out Exchanges.Exchange;
      Name : in Processed_Name;
      Data : in S_Expressions.Atom);
      --  Append Data to Exchange, ignoring Name

   function Create (Name : S_Expressions.Atom) return Processed_Name;
      --  Build a processed name from Name

   function Create_Ref (Data : in S_Expressions.Atom)
     return S_Expressions.Atom_Refs.Immutable_Reference
     renames S_Expressions.Atom_Ref_Constructors.Create;
      --  Create an atom reference

   function Name_Components
     (Name : Processed_Name;
      First_Component, Last_Component : S_Expressions.Offset)
     return S_Expressions.Atom
     with Pre => First_Component in 0 .. Name.Separator_Count + 1
         and then Last_Component in 0 .. Name.Separator_Count + 1;
      --  Return the selected path components from Name

   procedure Render_Components
     (Exchange : in out Exchanges.Exchange;
      Name : in Processed_Name;
      First_Index : in S_Expressions.Atom;
      Arguments : in out S_Expressions.Lockable.Descriptor'Class);
      --  Render a range of path component from Name

   procedure Render_Contents
     (Exchange : in out Exchanges.Exchange;
      Tag : in Tag_Contents;
      Name : in S_Expressions.Atom;
      Arguments : in out S_Expressions.Lockable.Descriptor'Class);
      --  Render the given template element for Tag into Exchange

   procedure Render_Page
     (Exchange : in out Exchanges.Exchange;
      Position : in Page_Maps.Cursor;
      Expression : in out S_Expressions.Lockable.Descriptor'Class);
      --  Dispatch rendering to the visible element at Position

   procedure Render_Tag
     (Exchange : in out Exchanges.Exchange;
      Context : in Tag_List_Context;
      Tag_Name : in S_Expressions.Atom;
      Expression : in out S_Expressions.Lockable.Descriptor'Class);
      --  Look-up for the tag named Tag_Name and render it

   function To_Component_Index
     (Name : Processed_Name;
      Image : S_Expressions.Atom)
     return S_Expressions.Offset
     with Post => To_Component_Index'Result in 0 .. Name.Separator_Count + 1;
      --  Parse Image as a component number of Name



   procedure Add_To_List is new S_Expressions.Interpreter_Loop
     (Tag_List, Meaningless_Type, Add_Tags_To_List);
      --  Parse a tag list and append it to a Tag_List object

   procedure Render is new S_Expressions.Interpreter_Loop
     (Exchanges.Exchange, Processed_Name, Render_Components, Append);

   procedure Render is new S_Expressions.Interpreter_Loop
     (Exchanges.Exchange, Tag_Contents, Render_Contents, Append);

   procedure Render is new S_Expressions.Interpreter_Loop
     (Exchanges.Exchange, Tag_List_Context, Render_Tag);

   procedure Render_Pages is new List_Templates.Render
     (Page_Maps.Map_Iterator_Interfaces, Render_Page);



   ------------------------------
   -- Local Helper Subprograms --
   ------------------------------

   procedure Add_Tags_To_List
     (List : in out Tag_List;
      Context : in Meaningless_Type;
      Key : in S_Expressions.Atom;
      Expression : in out S_Expressions.Lockable.Descriptor'Class)
   is
      pragma Unreferenced (Context);
      use type S_Expressions.Events.Event;
      Event : S_Expressions.Events.Event := Expression.Current_Event;
      Item : Tag_Description;
   begin
      Item.Key := Create_Ref (Key);

      while Event = S_Expressions.Events.Add_Atom loop
         Item.Tag := Create_Ref (Expression.Current_Atom);
         Append (List, Item);
         Expression.Next (Event);
      end loop;
   end Add_Tags_To_List;


   procedure Append
     (List : in out Tag_List;
      Item : in Tag_Description) is
   begin
      if List.Internal.Is_Empty then
         List.Internal := Tag_Lists.Create (new Tag_Array'(1 => Item));
      else
         List.Internal := Tag_Lists.Create
           (new Tag_Array'(List.Internal.Query & Item));
      end if;
   end Append;


   procedure Append
     (Exchange : in out Exchanges.Exchange;
      Tag : in Tag_Contents;
      Data : in S_Expressions.Atom)
   is
      pragma Unreferenced (Tag);
   begin
      Exchanges.Append (Exchange, Data);
   end Append;


   procedure Append
     (Exchange : in out Exchanges.Exchange;
      Name : in Processed_Name;
      Data : in S_Expressions.Atom)
   is
      pragma Unreferenced (Name);
   begin
      Exchanges.Append (Exchange, Data);
   end Append;


   function Create (Name : S_Expressions.Atom) return Processed_Name is
      Index, Count : S_Expressions.Count := 0;
   begin
      for O of Name loop
         if O = Path_Separator then
            Count := Count + 1;
         end if;
      end loop;

      return Result : Processed_Name
        := (Size => Name'Length,
            Separator_Count => Count,
            Full => Name,
            Separators => <>)
      do
         for I in Result.Full'Range loop
            if Result.Full (I) = Path_Separator then
               Index := Index + 1;
               Result.Separators (Index) := I;
            end if;
         end loop;

         pragma Assert (Index = Count);
      end return;
   end Create;


   function Name_Components
     (Name : Processed_Name;
      First_Component, Last_Component : S_Expressions.Offset)
     return S_Expressions.Atom
   is
      First : S_Expressions.Offset := Name.Full'First;
      Last : S_Expressions.Offset := Name.Full'Last;
   begin
      if First_Component = 0 or Last_Component = 0 then
         return S_Expressions.Null_Atom;
      end if;

      if First_Component - 1 in Name.Separators'Range then
         First := Name.Separators (First_Component - 1) + 1;
      end if;

      if Last_Component in Name.Separators'Range then
         Last := Name.Separators (Last_Component) - 1;
      end if;

      return Name.Full (First .. Last);
   end Name_Components;


   procedure Render_Components
     (Exchange : in out Exchanges.Exchange;
      Name : in Processed_Name;
      First_Index : in S_Expressions.Atom;
      Arguments : in out S_Expressions.Lockable.Descriptor'Class)
   is
      use type S_Expressions.Events.Event;

      First : constant S_Expressions.Offset
        := To_Component_Index (Name, First_Index);
      Last : constant S_Expressions.Offset
        := (if Arguments.Current_Event = S_Expressions.Events.Add_Atom
            then To_Component_Index (Name, Arguments.Current_Atom)
            else First);
   begin
      if First > 0 and Last > 0 then
         Exchanges.Append (Exchange, Name_Components (Name, First, Last));
      end if;
   end Render_Components;


   procedure Render_Contents
     (Exchange : in out Exchanges.Exchange;
      Tag : in Tag_Contents;
      Name : in S_Expressions.Atom;
      Arguments : in out S_Expressions.Lockable.Descriptor'Class)
   is
      package Commands renames Natools.Static_Maps.Web.Tags;
   begin
      case Commands.To_List_Command (S_Expressions.To_String (Name)) is
         when Commands.Unknown_List_Command =>
            Log (Severities.Error, "Unknown tag template element """
              & S_Expressions.To_String (Name) & '"');

         when Commands.All_Elements =>
            Render_Pages
              (Exchange,
               Tag_Maps.Element (Tag.Position).Iterate,
               List_Templates.Read_Parameters (Arguments));

         when Commands.Full_Name =>
            Exchanges.Append (Exchange, Tag_Maps.Key (Tag.Position));

         when Commands.Last_Name =>
            declare
               Full_Name : constant S_Expressions.Atom
                 := Tag_Maps.Key (Tag.Position);
               I : S_Expressions.Offset := Full_Name'Last;
            begin
               while I in Full_Name'Range
                 and then Full_Name (I) /= Path_Separator
               loop
                  I := I - 1;
               end loop;

               Exchanges.Append
                 (Exchange,
                  Full_Name (I + 1 .. Full_Name'Last));
            end;

         when Commands.Name =>
            Render (Arguments, Exchange, Create (Tag_Maps.Key (Tag.Position)));
      end case;
   end Render_Contents;


   procedure Render_Tag
     (Exchange : in out Exchanges.Exchange;
      Context : in Tag_List_Context;
      Tag_Name : in S_Expressions.Atom;
      Expression : in out S_Expressions.Lockable.Descriptor'Class)
   is
      Tag : constant Tag_Contents
        := Get_Tag (Context.DB, Tag_Name, Context.Parent_Tags);
   begin
      if Is_Empty (Tag) then
         Log (Severities.Error, "Unable to find tag """
           & S_Expressions.To_String (Tag_Name) & '"');
         return;
      end if;

      Render (Exchange, Tag, Expression);
   end Render_Tag;


   procedure Render_Page
     (Exchange : in out Exchanges.Exchange;
      Position : in Page_Maps.Cursor;
      Expression : in out S_Expressions.Lockable.Descriptor'Class) is
   begin
      Render (Exchange, Page_Maps.Element (Position), Expression);
   end Render_Page;


   function To_Component_Index
     (Name : Processed_Name;
      Image : S_Expressions.Atom)
     return S_Expressions.Offset
   is
      function To_Number (Img : S_Expressions.Atom)
        return S_Expressions.Offset;

      function To_Number (Img : S_Expressions.Atom)
        return S_Expressions.Offset
      is
         Digit, Result : S_Expressions.Offset := 0;
      begin
         for D of Img loop
            Digit := S_Expressions.Offset (D - Character'Pos ('0'));

            if Digit in 0 .. 9 then
               Result := Result * 10 + Digit;
            else
               Log (Severities.Error, "Invalid path component index """
                 & S_Expressions.To_String (Image) & """ for"
                 & S_Expressions.Count'Image (Name.Separator_Count + 1)
                 & "-component full name """
                 & S_Expressions.To_String (Name.Full) & '"');
               return 0;
            end if;
         end loop;

         return Result;
      end To_Number;

      Result : S_Expressions.Offset := 0;
   begin
      if Image = S_Expressions.To_Atom ("first") then
         return 1;
      elsif Image = S_Expressions.To_Atom ("last") then
         return Name.Separator_Count + 1;
      end if;

      if Image'Length > 0 then
         if Image (Image'First) = Character'Pos ('-') then
            Result := To_Number (Image (Image'First + 1 .. Image'Last));

            if Result > 0 then
               Result := Name.Separator_Count + 2 - Result;
            end if;
         else
            Result := To_Number (Image);
         end if;
      end if;

      if Result not in 0 .. Name.Separator_Count + 1 then
         Result := 0;
      end if;

      return Result;
   end To_Component_Index;



   ------------------------
   -- Tag_List Interface --
   ------------------------

   procedure Clear (List : in out Tag_List) is
   begin
      List.Internal.Reset;
   end Clear;


   procedure Append
     (List : in out Tag_List;
      Expression : in out S_Expressions.Lockable.Descriptor'Class) is
   begin
      Add_To_List (Expression, List, Meaningless_Value);
   end Append;



   ------------------------------
   -- Tag_DB_Builder Interface --
   ------------------------------

   procedure Add_Entry
     (Builder : in out Tag_DB_Builder;
      Tag : in S_Expressions.Atom;
      Key : in S_Expressions.Atom;
      Element : in Visible'Class)
   is
      Map_Position : Builder_Maps.Cursor;
      Inserted : Boolean;
   begin
      Builder.Internal.Insert
        (Tag, Page_Maps.Unsafe_Maps.Empty_Map,
         Map_Position, Inserted);
      Builder.Internal.Reference (Map_Position).Insert (Key, Element);
   end Add_Entry;


   procedure Add_To_Tag
     (Builder : in out Tag_DB_Builder;
      Tag : in S_Expressions.Atom;
      Key : in S_Expressions.Atom;
      Element : in Visible'Class)
   is
      Last : S_Expressions.Offset := Tag'Last;
   begin
      Insert :
      loop
         Add_Entry (Builder, Tag (Tag'First .. Last), Key, Element);

         Find_Parent :
         loop
            Last := Last - 1;
            exit Insert when Last not in Tag'Range;
            exit Find_Parent when Tag (Last) = Path_Separator;
         end loop Find_Parent;

         Last := Last - 1;
      end loop Insert;
   end Add_To_Tag;


   function Create (Builder : Tag_DB_Builder) return Tag_Maps.Constant_Map is
      Map : Tag_Maps.Unsafe_Maps.Map;
   begin
      for Cursor in Builder.Internal.Iterate loop
         Map.Insert
           (Builder_Maps.Key (Cursor),
            Page_Maps.Create (Builder.Internal (Cursor)));
      end loop;

      return Tag_Maps.Create (Map);
   end Create;


   procedure Register
     (Builder : in out Tag_DB_Builder;
      Keys : in Tag_List;
      Element : in Visible'Class) is
   begin
      if not Keys.Internal.Is_Empty then
         for Descr of Keys.Internal.Query loop
            Add_To_Tag (Builder, Descr.Tag.Query, Descr.Key.Query, Element);
         end loop;
      end if;
   end Register;



   ----------------------
   -- Tag_DB Interface --
   ----------------------

   function Create (Builder : Tag_DB_Builder) return Tag_DB is
   begin
      return (Internal => Create (Builder));
   end Create;


   procedure Render
     (Exchange : in out Exchanges.Exchange;
      DB : in Tag_DB;
      Expression : in out S_Expressions.Lockable.Descriptor'Class;
      Parent_Tags : in Tag_List := Empty_Tag_List)
   is
      Context : constant Tag_List_Context := (DB, Parent_Tags);
   begin
      case Expression.Current_Event is
         when S_Expressions.Events.Add_Atom =>
            declare
               Name : constant S_Expressions.Atom := Expression.Current_Atom;
            begin
               Expression.Next;
               Render_Tag (Exchange, Context, Name, Expression);
            end;

         when S_Expressions.Events.Open_List =>
            Render (Expression, Exchange, Context);

         when others =>
            null;
      end case;
   end Render;


   procedure Reset
     (DB : in out Tag_DB;
      Builder : in Tag_DB_Builder) is
   begin
      DB.Internal := Create (Builder);
   end Reset;



   ----------------------------
   -- Tag_Contents Interface --
   ----------------------------

   function Get_Tag
     (DB : in Tag_DB;
      Name : in S_Expressions.Atom;
      Parent_Tags : in Tag_List := Empty_Tag_List)
     return Tag_Contents
   is
      Tag : Tag_Description := Empty_Description;
   begin
      if not Parent_Tags.Internal.Is_Empty then
         for T of Parent_Tags.Internal.Query loop
            if Is_Path_Prefix (Name, T.Tag.Query) then
               Tag := T;
               exit;
            end if;
         end loop;
      end if;

      return
        (Position => DB.Internal.Find (Name),
         Parent => Tag);
   end Get_Tag;


   procedure Render
     (Exchange : in out Exchanges.Exchange;
      Tag : in Tag_Contents;
      Expression : in out S_Expressions.Lockable.Descriptor'Class) is
   begin
      Render (Expression, Exchange, Tag);
   end Render;

end Natools.Web.Tags;
