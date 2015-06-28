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
with Natools.S_Expressions.Printers;
with Natools.Time_IO.RFC_3339;

package body Natools.Web.Containers is

   procedure Add_Atom
     (List : in out Unsafe_Atom_Lists.List;
      Context : in Meaningless_Type;
      Atom : in S_Expressions.Atom);
      --  Append a new atom to List

   procedure Add_Date
     (Map : in out Date_Maps.Unsafe_Maps.Map;
      Context : in Meaningless_Type;
      Name : in S_Expressions.Atom;
      Value : in out S_Expressions.Lockable.Descriptor'Class);
      --  Append a new named date to Map

   procedure Add_Expression
     (Map : in out Expression_Maps.Unsafe_Maps.Map;
      Context : in Meaningless_Type;
      Name : in S_Expressions.Atom;
      Value : in out S_Expressions.Lockable.Descriptor'Class);
      --  Insert a new node (Name -> Cache (Value)) in Map

   procedure Add_Expression_Map
     (Map : in out Expression_Map_Maps.Unsafe_Maps.Map;
      Context : in Meaningless_Type;
      Name : in S_Expressions.Atom;
      Value : in out S_Expressions.Lockable.Descriptor'Class);
      --  Insert a new node (Name -> Expression_Map (Value)) in Map


   procedure Date_Reader is new S_Expressions.Interpreter_Loop
     (Date_Maps.Unsafe_Maps.Map, Meaningless_Type, Add_Date);

   procedure List_Reader is new S_Expressions.Interpreter_Loop
     (Unsafe_Atom_Lists.List, Meaningless_Type,
      Dispatch_Without_Argument => Add_Atom);

   procedure Map_Map_Reader is new S_Expressions.Interpreter_Loop
     (Expression_Map_Maps.Unsafe_Maps.Map, Meaningless_Type,
      Add_Expression_Map);

   procedure Map_Reader is new S_Expressions.Interpreter_Loop
     (Expression_Maps.Unsafe_Maps.Map, Meaningless_Type, Add_Expression);


   ------------------------------
   -- Local Helper Subprograms --
   ------------------------------

   procedure Add_Atom
     (List : in out Unsafe_Atom_Lists.List;
      Context : in Meaningless_Type;
      Atom : in S_Expressions.Atom)
   is
      pragma Unreferenced (Context);
   begin
      List.Append (Atom);
   end Add_Atom;


   procedure Add_Date
     (Map : in out Date_Maps.Unsafe_Maps.Map;
      Context : in Meaningless_Type;
      Name : in S_Expressions.Atom;
      Value : in out S_Expressions.Lockable.Descriptor'Class)
   is
      pragma Unreferenced (Context);
      use type S_Expressions.Events.Event;
      Item : Date;
   begin
      if Value.Current_Event = S_Expressions.Events.Add_Atom then
         declare
            Image : constant String
              := S_Expressions.To_String (Value.Current_Atom);
         begin
            if Time_IO.RFC_3339.Is_Valid (Image) then
               Time_IO.RFC_3339.Value (Image, Item.Time, Item.Offset);
               Map.Include (Name, Item);
            else
               Log (Severities.Warning, "Ignoring invalid date named """
                 & S_Expressions.To_String (Name) & '"');
            end if;
         end;
      else
         Map.Exclude (Name);
      end if;
   end Add_Date;


   procedure Add_Expression
     (Map : in out Expression_Maps.Unsafe_Maps.Map;
      Context : in Meaningless_Type;
      Name : in S_Expressions.Atom;
      Value : in out S_Expressions.Lockable.Descriptor'Class)
   is
      pragma Unreferenced (Context);
      Expression : S_Expressions.Caches.Reference;
   begin
      S_Expressions.Printers.Transfer (Value, Expression);
      Map.Include (Name, Expression.First);
   end Add_Expression;


   procedure Add_Expression_Map
     (Map : in out Expression_Map_Maps.Unsafe_Maps.Map;
      Context : in Meaningless_Type;
      Name : in S_Expressions.Atom;
      Value : in out S_Expressions.Lockable.Descriptor'Class)
   is
      pragma Unreferenced (Context);
      Expression_Map : Expression_Maps.Constant_Map;
   begin
      if Map.Contains (Name) then
         Log (Severities.Error,
           "Duplicate name """ & S_Expressions.To_String (Name)
           & """ in expression map map");
         return;
      end if;

      Set_Expressions (Expression_Map, Value);
      Map.Insert (Name, Expression_Map);
   end Add_Expression_Map;



   --------------------
   -- Map Interfaces --
   --------------------

   procedure Set_Dates
     (Map : in out Date_Maps.Constant_Map;
      Date_List : in out S_Expressions.Lockable.Descriptor'Class)
   is
      New_Map : Date_Maps.Unsafe_Maps.Map;
   begin
      Date_Reader (Date_List, New_Map, Meaningless_Value);
      Map.Replace (New_Map);
   end Set_Dates;


   procedure Set_Expressions
     (Map : in out Expression_Maps.Constant_Map;
      Expression_List : in out S_Expressions.Lockable.Descriptor'Class)
   is
      New_Map : Expression_Maps.Unsafe_Maps.Map;
   begin
      Map_Reader (Expression_List, New_Map, Meaningless_Value);
      Map.Replace (New_Map);
   end Set_Expressions;


   procedure Set_Expression_Maps
     (Map : in out Expression_Map_Maps.Constant_Map;
      Expression_Map_List : in out S_Expressions.Lockable.Descriptor'Class)
   is
      New_Map : Expression_Map_Maps.Unsafe_Maps.Map;
   begin
      Map_Map_Reader (Expression_Map_List, New_Map, Meaningless_Value);
      Map.Replace (New_Map);
   end Set_Expression_Maps;



   ---------------------------
   -- Atom Arrays Interface --
   ---------------------------

   procedure Append_Atoms
     (Target : in out Unsafe_Atom_Lists.List;
      Expression : in out S_Expressions.Lockable.Descriptor'Class) is
   begin
      List_Reader (Expression, Target, Meaningless_Value);
   end Append_Atoms;


   function Create (Source : Unsafe_Atom_Lists.List)
     return Atom_Array_Refs.Immutable_Reference
   is
      function Create_Array return Atom_Array;

      function Create_Array return Atom_Array is
         Cursor : Unsafe_Atom_Lists.Cursor := Source.First;
         Result : Atom_Array (1 .. S_Expressions.Count (Source.Length));
      begin
         for I in Result'Range loop
            Result (I) := S_Expressions.Atom_Ref_Constructors.Create
              (Unsafe_Atom_Lists.Element (Cursor));
            Unsafe_Atom_Lists.Next (Cursor);
         end loop;

         return Result;
      end Create_Array;
   begin
      return Atom_Array_Refs.Create (Create_Array'Access);
   end Create;

end Natools.Web.Containers;
