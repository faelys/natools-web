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

package body Natools.Web.Tags is

   use type S_Expressions.Atom;
   use type S_Expressions.Octet;
   use type S_Expressions.Offset;

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

   function Create_Ref (Data : in S_Expressions.Atom)
     return S_Expressions.Atom_Refs.Immutable_Reference
     renames S_Expressions.Atom_Ref_Constructors.Create;
      --  Create an atom reference


   ------------------------------
   -- Local Helper Subprograms --
   ------------------------------

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


   procedure Add_To_List is new S_Expressions.Interpreter_Loop
     (Tag_List, Meaningless_Type, Add_Tags_To_List);



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

end Natools.Web.Tags;
