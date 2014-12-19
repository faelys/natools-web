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
with Natools.Web.Exchanges;
with Natools.Web.List_Templates;
with Natools.Web.Sites;

package body Natools.Web.Tags is

   use type S_Expressions.Atom;
   use type S_Expressions.Octet;
   use type S_Expressions.Offset;

   package Atom_Maps is new Ada.Containers.Indefinite_Ordered_Maps
     (S_Expressions.Atom, S_Expressions.Atom, S_Expressions.Less_Than);

   type Offset_Array is
     array (S_Expressions.Count range <>) of S_Expressions.Offset;

   type Processed_Name
     (Size, Separator_Count : S_Expressions.Count)
   is record
      Full : S_Expressions.Atom (1 .. Size);
      Separators : Offset_Array (1 .. Separator_Count);
   end record;

   type Tag_DB_Context is record
      DB : Tag_DB;
      Parent_Tags : Tag_List;
   end record;


   Path_Separator : constant S_Expressions.Octet := Character'Pos ('/');


   function Is_Prefix (Small, Large : S_Expressions.Atom) return Boolean
     is (Small'Length <= Large'Length
     and then Small = Large (Large'First .. Large'First + Small'Length - 1));
      --  Check whether Small is a prefix of Large

   function Is_Path_Prefix (Small, Large : S_Expressions.Atom) return Boolean
     is (Is_Prefix (Small, Large)
         and then (Small'Length = Large'Length
            or else Large (Large'First + Small'Length) = Path_Separator));
      --  Check whether Small is a parent path of Large


   procedure Add_Tags_To_List
     (List : in out Tag_List;
      Context : in Meaningless_Type;
      Key : in S_Expressions.Atom;
      Expression : in out S_Expressions.Lockable.Descriptor'Class);
      --  Parse Expression for tag names and add (tag, key) to List

   function Adjust
     (Position : Tag_List_Cursor;
      Prefix : S_Expressions.Atom;
      Proceed : not null access procedure (Pos : in out Tag_List_Cursor))
     return Tag_List_Cursor;
      --  Repeatedly apply Proceed to Position until it has the correct prefix

   procedure Append
     (List : in out Tag_List;
      Item : in Tag_Description);
      --  Append a single tag to List.
      --  This is a simple O(n) copy, which leads to quadratic list
      --  construction. This is considered acceptable since lists are expected
      --  to be small and seldom (re)built.

   procedure Append
     (Exchange : in out Sites.Exchange;
      Tag : in Tag_Contents;
      Data : in S_Expressions.Atom);
      --  Append Data to Exchange, ignoring Tag

   procedure Append
     (Exchange : in out Sites.Exchange;
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
     (Exchange : in out Sites.Exchange;
      Name : in Processed_Name;
      First_Index : in S_Expressions.Atom;
      Arguments : in out S_Expressions.Lockable.Descriptor'Class);
      --  Render a range of path component from Name

   procedure Render_Contents
     (Exchange : in out Sites.Exchange;
      Tag : in Tag_Contents;
      Name : in S_Expressions.Atom;
      Arguments : in out S_Expressions.Lockable.Descriptor'Class);
      --  Render the given template element for Tag into Exchange

   procedure Render_Page
     (Exchange : in out Sites.Exchange;
      Position : in Page_Maps.Cursor;
      Expression : in out S_Expressions.Lockable.Descriptor'Class);
      --  Dispatch rendering to the visible element at Position

   procedure Render_Tag
     (Exchange : in out Sites.Exchange;
      Context : in Tag_DB_Context;
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
     (Sites.Exchange, Processed_Name, Render_Components, Append);

   procedure Render is new S_Expressions.Interpreter_Loop
     (Sites.Exchange, Tag_Contents, Render_Contents, Append);

   procedure Render is new S_Expressions.Interpreter_Loop
     (Sites.Exchange, Tag_DB_Context, Render_Tag);

   procedure Render_Pages is new List_Templates.Render
     (Page_Maps.Map_Iterator_Interfaces, Render_Page);

   procedure Render_List is new List_Templates.Render
     (Tag_List_Iterators);



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


   function Adjust
     (Position : Tag_List_Cursor;
      Prefix : S_Expressions.Atom;
      Proceed : not null access procedure (Pos : in out Tag_List_Cursor))
     return Tag_List_Cursor
   is
      Result : Tag_List_Cursor := Position;
   begin
      while Has_Element (Result)
        and then not Is_Prefix (Prefix,
            Result.List.Query.Data (Result.Index).Tag.Query)
      loop
         Proceed (Result);
      end loop;

      return Result;
   end Adjust;


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
     (Exchange : in out Sites.Exchange;
      Tag : in Tag_Contents;
      Data : in S_Expressions.Atom)
   is
      pragma Unreferenced (Tag);
   begin
      Exchanges.Append (Exchange, Data);
   end Append;


   procedure Append
     (Exchange : in out Sites.Exchange;
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
     (Exchange : in out Sites.Exchange;
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
     (Exchange : in out Sites.Exchange;
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

         when Commands.Current_Element =>
            if Page_Maps.Has_Element (Tag.Current_Element) then
               Render
                 (Exchange,
                  Page_Maps.Element (Tag.Current_Element),
                  Arguments);
            else
               Log (Severities.Error,
                 "Current_Element called when it's empty");
            end if;

         when Commands.Greater_Elements =>
            if Page_Maps.Has_Element (Tag.Current_Element) then
               declare
                  Page_Map : constant Page_Maps.Constant_Map
                    := Tag_Maps.Element (Tag.Position);
               begin
                  Render_Pages
                    (Exchange,
                     Page_Map.Iterate
                       (Page_Maps.Next (Tag.Current_Element),
                        Page_Map.Last),
                     List_Templates.Read_Parameters (Arguments));
               end;
            else
               Log (Severities.Error,
                 "Greater_Elements called without current element");
            end if;

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

         when Commands.Lesser_Elements =>
            if Page_Maps.Has_Element (Tag.Current_Element) then
               declare
                  Page_Map : constant Page_Maps.Constant_Map
                    := Tag_Maps.Element (Tag.Position);
               begin
                  Render_Pages
                    (Exchange,
                     Page_Map.Iterate
                       (Page_Map.First,
                        Page_Maps.Previous (Tag.Current_Element)),
                     List_Templates.Read_Parameters (Arguments));
               end;
            else
               Log (Severities.Error,
                 "Lesser_Elements called without current element");
            end if;

         when Commands.Name =>
            Render (Arguments, Exchange, Create (Tag_Maps.Key (Tag.Position)));
      end case;
   end Render_Contents;


   procedure Render_Tag
     (Exchange : in out Sites.Exchange;
      Context : in Tag_DB_Context;
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
     (Exchange : in out Sites.Exchange;
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


   overriding function First (Iterator : Tag_List_Iterator)
     return Tag_List_Cursor is
   begin
      if Iterator.List.Is_Empty
        or else Iterator.List.Query.Data'Length = 0
      then
         return No_Element;
      else
         return Adjust
           ((DB => Iterator.DB,
             List => Iterator.List,
             Index => Iterator.List.Query.Data'First),
            Iterator.Prefix.Query,
            Next'Access);
      end if;
   end First;


   overriding function Last (Iterator : Tag_List_Iterator)
     return Tag_List_Cursor is
   begin
      if Iterator.List.Is_Empty
        or else Iterator.List.Query.Data'Length = 0
      then
         return No_Element;
      else
         return Adjust
           ((DB => Iterator.DB,
             List => Iterator.List,
             Index => Iterator.List.Query.Data'Last),
            Iterator.Prefix.Query,
            Previous'Access);
      end if;
   end Last;


   procedure Next (Position : in out Tag_List_Cursor) is
   begin
      if Has_Element (Position) then
         Position.Index := Position.Index + 1;
      end if;
   end Next;


   overriding function Next
     (Iterator : Tag_List_Iterator;
      Position : Tag_List_Cursor)
     return Tag_List_Cursor
   is
      Result : Tag_List_Cursor := Position;
   begin
      pragma Assert (not Has_Element (Position)
        or else (Iterator.DB = Position.DB
           and then Tag_Lists."=" (Iterator.List, Position.List)));

      Next (Result);

      return Adjust (Result, Iterator.Prefix.Query, Next'Access);
   end Next;


   procedure Previous (Position : in out Tag_List_Cursor) is
   begin
      if Has_Element (Position) then
         Position.Index := Position.Index - 1;
      end if;
   end Previous;


   overriding function Previous
     (Iterator : Tag_List_Iterator;
      Position : Tag_List_Cursor)
     return Tag_List_Cursor
   is
      Result : Tag_List_Cursor := Position;
   begin
      pragma Assert (not Has_Element (Position)
        or else (Iterator.DB = Position.DB
           and then Tag_Lists."=" (Iterator.List, Position.List)));

      Previous (Result);

      return Adjust (Result, Iterator.Prefix.Query, Previous'Access);
   end Previous;


   procedure Render
     (Exchange : in out Sites.Exchange;
      Position : in Tag_List_Cursor;
      Expression : in out S_Expressions.Lockable.Descriptor'Class) is
   begin
      Render
        (Expression,
         Exchange,
         Get_Tag
           (Position.DB,
            Position.List.Query.Data (Position.Index).Tag.Query,
            (Internal => Position.List)));
   end Render;



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
      Element_Position : Page_Maps.Unsafe_Maps.Cursor;
      Fallback_Key : S_Expressions.Atom := Key & (1 => 0);
      Inserted : Boolean;
   begin
      Builder.Internal.Insert
        (Tag, Page_Maps.Unsafe_Maps.Empty_Map,
         Map_Position, Inserted);

      declare
         Ref : constant Builder_Maps.Reference_Type
           := Builder.Internal.Reference (Map_Position);
      begin
         Ref.Insert (Key, Element, Element_Position, Inserted);

         while not Inserted loop
            Ref.Insert (Fallback_Key, Element, Element_Position, Inserted);
            Fallback_Key (Fallback_Key'Last)
              := Fallback_Key (Fallback_Key'Last) + 1;

            if Fallback_Key (Fallback_Key'Last) = 0 then
               Log (Severities.Error, "Unable to insert into tag """
                  & S_Expressions.To_String (Tag)
                  & """ with key """
                  & S_Expressions.To_String (Key)
                  & """: all fallback possibilities are exhausted");
               exit;
            end if;
         end loop;
      end;
   end Add_Entry;


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
      Element : in Visible'Class)
   is
      procedure Insert_Or_Log
        (Container : in out Atom_Maps.Map;
         Tag, Key : in S_Expressions.Atom;
         Tag_Kind : in String;
         Severity : in Severities.Code);

      procedure Insert_Or_Log
        (Container : in out Atom_Maps.Map;
         Tag, Key : in S_Expressions.Atom;
         Tag_Kind : in String;
         Severity : in Severities.Code)
      is
         Position : Atom_Maps.Cursor;
         Inserted : Boolean;
      begin
         Container.Insert (Tag, Key, Position, Inserted);

         if not Inserted and then Atom_Maps.Element (Position) /= Key then
            pragma Assert (Tag = Atom_Maps.Key (Position));
            Log (Severity,
              "Registering element to " & Tag_Kind & " tag """
              & S_Expressions.To_String (Tag)
              & """ with conflicting keys """
              & S_Expressions.To_String (Atom_Maps.Element (Position))
              & """ and """
              & S_Expressions.To_String (Key)
              & """, dropping the latter");
         end if;
      end Insert_Or_Log;

      Explicit, Implicit : Atom_Maps.Map;
      Last : S_Expressions.Offset;
   begin
      if Keys.Internal.Is_Empty then
         return;
      end if;

      Process_Explicit :
      for Descr of Keys.Internal.Query loop
         Insert_Or_Log
           (Explicit,
            Descr.Tag.Query, Descr.Key.Query,
            "explicit", Severities.Error);
      end loop Process_Explicit;

      Process_Implicit :
      for Descr of Keys.Internal.Query loop
         declare
            Key : constant S_Expressions.Atom := Descr.Key.Query;
            Tag : constant S_Expressions.Atom := Descr.Tag.Query;
         begin
            Last := Tag'Last;

            Insert_Pairs :
            loop
               Find_Parent :
               loop
                  Last := Last - 1;
                  exit Insert_Pairs when Last not in Tag'Range;
                  exit Find_Parent when Tag (Last) = Path_Separator;
               end loop Find_Parent;

               Last := Last - 1;

               if not Explicit.Contains (Tag (Tag'First .. Last)) then
                  Insert_Or_Log
                    (Implicit,
                     Tag (Tag'First .. Last), Key,
                     "implicit", Severities.Warning);
               end if;
            end loop Insert_Pairs;
         end;
      end loop Process_Implicit;

      for Cursor in Explicit.Iterate loop
         Add_Entry
           (Builder,
            Atom_Maps.Key (Cursor),
            Atom_Maps.Element (Cursor),
            Element);
      end loop;

      for Cursor in Implicit.Iterate loop
         Add_Entry
           (Builder,
            Atom_Maps.Key (Cursor),
            Atom_Maps.Element (Cursor),
            Element);
      end loop;
   end Register;



   ----------------------
   -- Tag_DB Interface --
   ----------------------

   function Create (Builder : Tag_DB_Builder) return Tag_DB is
   begin
      return (Internal => Create (Builder));
   end Create;


   procedure Render
     (Exchange : in out Sites.Exchange;
      DB : in Tag_DB;
      Expression : in out S_Expressions.Lockable.Descriptor'Class;
      Parent_Tags : in Tag_List := Empty_Tag_List)
   is
      Context : constant Tag_DB_Context := (DB, Parent_Tags);
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


   procedure Render
     (Exchange : in out Sites.Exchange;
      List : in Tag_List;
      DB : in Tag_DB;
      Prefix : in S_Expressions.Atom;
      Expression : in out S_Expressions.Lockable.Descriptor'Class) is
   begin
      Render_List
        (Exchange,
         Tag_List_Iterator'
           (DB => DB,
            List => List.Internal,
            Prefix => Create_Ref (Prefix)),
         List_Templates.Read_Parameters (Expression));
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
      Descr : Tag_Description := Empty_Description;
      Result : Tag_Contents
        := (Position => DB.Internal.Find (Name),
            Current_Element => Page_Maps.No_Element);
   begin
      if not Parent_Tags.Internal.Is_Empty
        and then Tag_Maps.Has_Element (Result.Position)
      then
         for T of Parent_Tags.Internal.Query loop
            if Name = T.Tag.Query then
               Descr := T;
               exit;
            end if;
         end loop;

         if Descr.Tag.Is_Empty then
            for T of Parent_Tags.Internal.Query loop
               if Is_Path_Prefix (Name, T.Tag.Query) then
                  Descr := T;
                  exit;
               end if;
            end loop;
         end if;

         if not Descr.Key.Is_Empty then
            Result.Current_Element
              := Tag_Maps.Element (Result.Position).Find (Descr.Key.Query);
            pragma Assert (Page_Maps.Has_Element (Result.Current_Element));
         end if;
      end if;

      return Result;
   end Get_Tag;


   procedure Render
     (Exchange : in out Sites.Exchange;
      Tag : in Tag_Contents;
      Expression : in out S_Expressions.Lockable.Descriptor'Class) is
   begin
      Render (Expression, Exchange, Tag);
   end Render;

end Natools.Web.Tags;
