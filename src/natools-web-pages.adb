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

with Natools.S_Expressions.File_Readers;
with Natools.S_Expressions.Interpreter_Loop;
with Natools.S_Expressions.Lockable;
with Natools.S_Expressions.Atom_Ref_Constructors;
with Natools.Static_Maps.Web.Pages;

package body Natools.Web.Pages is

   procedure Execute
     (Data : in out Page_Data;
      Context : in Meaningless_Type;
      Name : in S_Expressions.Atom;
      Arguments : in out S_Expressions.Lockable.Descriptor'Class);


   ---------------------------
   -- Page Data Constructor --
   ---------------------------

   procedure Execute
     (Data : in out Page_Data;
      Context : in Meaningless_Type;
      Name : in S_Expressions.Atom;
      Arguments : in out S_Expressions.Lockable.Descriptor'Class)
   is
      pragma Unreferenced (Context);
      package Components renames Natools.Static_Maps.Web.Pages;
   begin
      case Components.To_Component (S_Expressions.To_String (Name)) is
         when Components.Error =>
            Log (Severities.Error, "Unknown page component """
              & S_Expressions.To_String (Name) & '"');

         when Components.Elements =>
            Containers.Set_Expressions (Data.Elements, Arguments);
      end case;
   end Execute;


   procedure Read_Page is new S_Expressions.Interpreter_Loop
     (Page_Data, Meaningless_Type, Execute);



   ----------------------
   -- Public Interface --
   ----------------------

   function Create (File_Path, Web_Path : in S_Expressions.Atom)
     return Page_Ref
   is
      function Create_Page return Page_Data;

      function Create_Page return Page_Data is
         Reader : Natools.S_Expressions.File_Readers.S_Reader
           := Natools.S_Expressions.File_Readers.Reader
              (S_Expressions.To_String (File_Path));
      begin
         return Result : Page_Data
           := (File_Path =>
                  S_Expressions.Atom_Ref_Constructors.Create (File_Path),
               Web_Path =>
                  S_Expressions.Atom_Ref_Constructors.Create (Web_Path),
               Elements => <>)
         do
            Read_Page (Reader, Result, Meaningless_Value);
         end return;
      end Create_Page;
   begin
      return (Ref => Data_Refs.Create (Create_Page'Access));
   end Create;

end Natools.Web.Pages;
