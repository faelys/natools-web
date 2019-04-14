------------------------------------------------------------------------------
-- Copyright (c) 2019, Natacha Porté                                        --
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
with Natools.S_Expressions.Enumeration_IO;
with Natools.S_Expressions.File_Readers;
with Natools.S_Expressions.Interpreter_Loop;
with Natools.S_Expressions.Special_Descriptors;
with Natools.Web.Containers;

package body Natools.Web.Simple_Pages.Multipages is

   package Components is
      type Enum is
        (Error,
         Comment_List,
         Comment_Path_Prefix,
         Comment_Path_Suffix,
         Elements);
   end Components;

   package Component_IO is new S_Expressions.Enumeration_IO.Typed_IO
     (Components.Enum);

   procedure Build_And_Register
     (Builder : in out Sites.Site_Builder;
      Expression : in out S_Expressions.Lockable.Descriptor'Class;
      Defaults : in Default_Data;
      Root_Path : in S_Expressions.Atom;
      Path_Spec : in S_Expressions.Atom);

   function Comment_Path
     (Defaults : in Default_Data;
      Spec : in S_Expressions.Atom)
     return S_Expressions.Atom_Refs.Immutable_Reference
     with Pre => not Defaults.Comment_Path_Prefix.Is_Empty
                 and then Spec'Length >= 1;

   function Key_Path (Path, Spec : S_Expressions.Atom)
     return S_Expressions.Atom;

   procedure Merge_Elements
     (Mutator : in Data_Refs.Mutator;
      Elements : Containers.Expression_Maps.Constant_Map);

   procedure Update
     (Defaults : in out Default_Data;
      Context : in Meaningless_Type;
      Name : in S_Expressions.Atom;
      Arguments : in out S_Expressions.Lockable.Descriptor'Class);

   function Web_Path (Path, Spec : S_Expressions.Atom)
     return S_Expressions.Atom_Refs.Immutable_Reference;


   procedure Update_Defaults is new S_Expressions.Interpreter_Loop
     (Default_Data, Meaningless_Type, Update);



   ------------------------------
   -- Local Helper Subprograms --
   ------------------------------

   procedure Build_And_Register
     (Builder : in out Sites.Site_Builder;
      Expression : in out S_Expressions.Lockable.Descriptor'Class;
      Defaults : in Default_Data;
      Root_Path : in S_Expressions.Atom;
      Path_Spec : in S_Expressions.Atom)
   is
      Comment_Path_Override : S_Expressions.Atom_Refs.Immutable_Reference;
      Page : constant Page_Ref := Create (Expression);
   begin
      if not Defaults.Comment_Path_Prefix.Is_Empty then
         Comment_Path_Override := Comment_Path (Defaults, Path_Spec);
      end if;

      declare
         Mutator : constant Data_Refs.Mutator := Page.Ref.Update;
      begin
         if not Defaults.Elements.Is_Empty then
            Merge_Elements (Mutator, Defaults.Elements);
         end if;

         if Mutator.Comment_List.Is_Null then
            declare
               package Sx renames Natools.S_Expressions;
               Template : Sx.Caches.Cursor := Defaults.Comment_List;
               Empty : Sx.Special_Descriptors.Constant_Descriptor
                 (Sx.Events.End_Of_Input);
            begin
               Mutator.Comment_List.Set
                 (Template, Empty, Comment_Path_Override);
            end;
         end if;

         Mutator.File_Path := Defaults.File_Path;
         Mutator.Web_Path := Web_Path (Root_Path, Path_Spec);
      end;

      Register (Page, Builder, Key_Path (Root_Path, Path_Spec));
   end Build_And_Register;


   function Key_Path (Path, Spec : S_Expressions.Atom)
     return S_Expressions.Atom
   is
      use type S_Expressions.Atom;
      use type S_Expressions.Offset;
   begin
      case Spec (Spec'First) is
         when Character'Pos ('+') =>
            return Path & Spec (Spec'First + 1 .. Spec'Last);
         when Character'Pos ('-') | Character'Pos ('#') =>
            return S_Expressions.Null_Atom;
         when others =>
            return Spec;
      end case;
   end Key_Path;


   function Comment_Path
     (Defaults : in Default_Data;
      Spec : in S_Expressions.Atom)
     return S_Expressions.Atom_Refs.Immutable_Reference
   is
      use type S_Expressions.Atom;
      use type S_Expressions.Offset;

      Prefix : constant S_Expressions.Atom
        := Defaults.Comment_Path_Prefix.Query;
      Suffix : constant S_Expressions.Atom
        := (if Defaults.Comment_Path_Suffix.Is_Empty
            then S_Expressions.Null_Atom
            else Defaults.Comment_Path_Suffix.Query);
   begin
      case Spec (Spec'First) is
         when Character'Pos ('+')
            | Character'Pos ('-')
            | Character'Pos ('#')
         =>
            return S_Expressions.Atom_Ref_Constructors.Create
              (Prefix & Spec (Spec'First + 1 .. Spec'Last) & Suffix);
         when others =>
            return S_Expressions.Atom_Ref_Constructors.Create
              (Prefix & Spec & Suffix);
      end case;
   end Comment_Path;


   procedure Merge_Elements
     (Mutator : in Data_Refs.Mutator;
      Elements : Containers.Expression_Maps.Constant_Map)
   is
      Unsafe_Map : Containers.Expression_Maps.Unsafe_Maps.Map
        := Containers.Expression_Maps.To_Unsafe_Map (Mutator.Elements);
      Unused_Position : Containers.Expression_Maps.Unsafe_Maps.Cursor;
      Unused_Inserted : Boolean;
   begin
      for Cursor in Elements.Iterate loop
         Unsafe_Map.Insert
           (Containers.Expression_Maps.Key (Cursor),
            Containers.Expression_Maps.Element (Cursor),
            Unused_Position,
            Unused_Inserted);
      end loop;

      Mutator.Elements := Containers.Expression_Maps.Create (Unsafe_Map);
   end Merge_Elements;


   procedure Update
     (Defaults : in out Default_Data;
      Context : in Meaningless_Type;
      Name : in S_Expressions.Atom;
      Arguments : in out S_Expressions.Lockable.Descriptor'Class)
   is
      pragma Unreferenced (Context);
      use type S_Expressions.Events.Event;
   begin
      case Component_IO.Value (Name, Components.Error) is
         when Components.Error =>
            Log (Severities.Error, "Unknown multipage default component """
              & S_Expressions.To_String (Name) & '"');

         when Components.Comment_List =>
            Defaults.Comment_List
              := S_Expressions.Caches.Conditional_Move (Arguments);

         when Components.Comment_Path_Prefix =>
            if Arguments.Current_Event = S_Expressions.Events.Add_Atom then
               Defaults.Comment_Path_Prefix
                 := S_Expressions.Atom_Ref_Constructors.Create
                    (Arguments.Current_Atom);
            else
               Defaults.Comment_Path_Prefix.Reset;
            end if;

         when Components.Comment_Path_Suffix =>
            if Arguments.Current_Event = S_Expressions.Events.Add_Atom then
               Defaults.Comment_Path_Suffix
                 := S_Expressions.Atom_Ref_Constructors.Create
                    (Arguments.Current_Atom);
            else
               Defaults.Comment_Path_Suffix.Reset;
            end if;

         when Components.Elements =>
            Containers.Set_Expressions (Defaults.Elements, Arguments);
      end case;
   end Update;


   function Web_Path (Path, Spec : S_Expressions.Atom)
     return S_Expressions.Atom_Refs.Immutable_Reference
   is
      use type S_Expressions.Atom;
      use type S_Expressions.Offset;
   begin
      case Spec (Spec'First) is
         when Character'Pos ('+') | Character'Pos ('#') =>
            return S_Expressions.Atom_Ref_Constructors.Create
              (Path & Spec (Spec'First + 1 .. Spec'Last));
         when Character'Pos ('-') =>
            return S_Expressions.Atom_Ref_Constructors.Create
              (Spec (Spec'First + 1 .. Spec'Last));
         when others =>
            return S_Expressions.Atom_Ref_Constructors.Create (Spec);
      end case;
   end Web_Path;



   ----------------------
   -- Public Interface --
   ----------------------

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
      use type S_Expressions.Events.Event;

      Reader : Natools.S_Expressions.File_Readers.S_Reader
        := Natools.S_Expressions.File_Readers.Reader
           (S_Expressions.To_String (Object.File_Path.Query));
      Defaults : Default_Data;

      Lock : S_Expressions.Lockable.Lock_State;
      Event : S_Expressions.Events.Event := Reader.Current_Event;
   begin
      while Event = S_Expressions.Events.Open_List loop
         Reader.Lock (Lock);
         Reader.Next (Event);

         if Event = S_Expressions.Events.Add_Atom then
            declare
               Path_Spec : constant S_Expressions.Atom := Reader.Current_Atom;
            begin
               if Path_Spec'Length = 0 then
                  Update_Defaults (Reader, Defaults, Meaningless_Value);
               else
                  Build_And_Register
                    (Builder, Reader, Defaults, Path, Path_Spec);
               end if;
            end;
         end if;

         Reader.Unlock (Lock);
         Reader.Next (Event);
      end loop;
   end Load;

end Natools.Web.Simple_Pages.Multipages;
