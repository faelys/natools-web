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

with Ada.Directories;
with Ada.Streams.Stream_IO;
with Natools.File_Streams;
with Natools.S_Expressions.Atom_Ref_Constructors;

package body Natools.Web.Backends.Filesystem is

   package Stream_IO renames Ada.Streams.Stream_IO;

   function Compose
     (Self : File_Backend;
      Directory, Name : S_Expressions.Atom)
     return String;


   ------------------------------
   -- Local Helper Subprograms --
   ------------------------------

   function Compose
     (Self : File_Backend;
      Directory, Name : S_Expressions.Atom)
     return String is
   begin
      return Ada.Directories.Compose
        (S_Expressions.To_String (Self.Root.Query)
           & S_Expressions.To_String (Directory),
         S_Expressions.To_String (Name));
   end Compose;



   ----------------------
   -- Public Interface --
   ----------------------

   overriding function Append
     (Self : in out File_Backend;
      Directory, Name : in S_Expressions.Atom)
     return Ada.Streams.Root_Stream_Type'Class is
   begin
      return File_Streams.Open
        (Stream_IO.Append_File,
         Compose (Self, Directory, Name));
   end Append;


   not overriding function Create (Root : in String) return File_Backend is
   begin
      return (Root => S_Expressions.Atom_Ref_Constructors.Create
                        (S_Expressions.To_Atom (Root)));
   end Create;


   function Create
     (Arguments : in out S_Expressions.Lockable.Descriptor'Class)
     return Backend'Class is
   begin
      case Arguments.Current_Event is
         when S_Expressions.Events.Add_Atom =>
            return File_Backend'
              (Root => S_Expressions.Atom_Ref_Constructors.Create
                 (Arguments.Current_Atom));
         when others =>
            raise Constraint_Error with "File_System backend expects an atom";
      end case;
   end Create;


   overriding function Create
     (Self : in out File_Backend;
      Directory, Name : in S_Expressions.Atom)
     return Ada.Streams.Root_Stream_Type'Class is
   begin
      Ada.Directories.Create_Path
        (S_Expressions.To_String (Self.Root.Query)
           & S_Expressions.To_String (Directory));

      return File_Streams.Create
        (Stream_IO.Append_File,
         Compose (Self, Directory, Name));
   end Create;


   overriding procedure Delete
     (Self : in out File_Backend;
      Directory, Name : in S_Expressions.Atom) is
   begin
      Ada.Directories.Delete_File (Compose (Self, Directory, Name));
   end Delete;


   overriding procedure Iterate
     (Self : in File_Backend;
      Directory : in S_Expressions.Atom;
      Process : not null access procedure (Name : in S_Expressions.Atom))
   is
      Search : Ada.Directories.Search_Type;
      Directory_Entry : Ada.Directories.Directory_Entry_Type;
      Dir_Name : constant String
        := S_Expressions.To_String (Self.Root.Query)
           & S_Expressions.To_String (Directory);
   begin
      if not Ada.Directories.Exists (Dir_Name) then
         return;
      end if;

      Ada.Directories.Start_Search
        (Search => Search,
         Directory => Dir_Name,
         Pattern => "",
         Filter => (Ada.Directories.Ordinary_File => True, others => False));

      while Ada.Directories.More_Entries (Search) loop
         Ada.Directories.Get_Next_Entry (Search, Directory_Entry);
         Process.all
           (S_Expressions.To_Atom
              (Ada.Directories.Simple_Name (Directory_Entry)));
      end loop;

      Ada.Directories.End_Search (Search);
   end Iterate;


   overriding function Overwrite
     (Self : in out File_Backend;
      Directory, Name : in S_Expressions.Atom)
     return Ada.Streams.Root_Stream_Type'Class is
   begin
      return File_Streams.Open
        (Stream_IO.Out_File,
         Compose (Self, Directory, Name));
   end Overwrite;


   overriding function Read
     (Self : in File_Backend;
      Directory, Name : in S_Expressions.Atom)
     return Ada.Streams.Root_Stream_Type'Class is
   begin
      return File_Streams.Open
        (Stream_IO.In_File,
         Compose (Self, Directory, Name));
   end Read;

end Natools.Web.Backends.Filesystem;
