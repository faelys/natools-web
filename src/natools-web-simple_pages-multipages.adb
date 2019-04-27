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
with Natools.S_Expressions.File_Readers;

package body Natools.Web.Simple_Pages.Multipages is

   procedure Build_And_Register
     (Builder : in out Sites.Site_Builder;
      Expression : in out S_Expressions.Lockable.Descriptor'Class;
      Defaults : in Default_Data;
      Root_Path : in S_Expressions.Atom;
      Path_Spec : in S_Expressions.Atom);

   function Key_Path (Path, Spec : S_Expressions.Atom)
     return S_Expressions.Atom;

   function Web_Path (Path, Spec : S_Expressions.Atom)
     return S_Expressions.Atom_Refs.Immutable_Reference;



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
      use type S_Expressions.Offset;
      Name : constant S_Expressions.Atom
        := (if Path_Spec (Path_Spec'First) in
              Character'Pos ('+') | Character'Pos ('-') | Character'Pos ('#')
            then Path_Spec (Path_Spec'First + 1 .. Path_Spec'Last)
            else Path_Spec);
      Page : constant Page_Ref := Create (Expression, Defaults.Template, Name);
   begin
      declare
         Mutator : constant Data_Refs.Mutator := Page.Ref.Update;
      begin
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
                  Update (Defaults.Template, Reader);
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
