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

with Ada.Calendar.Formatting;
with Natools.S_Expressions.Atom_Ref_Constructors;
with Natools.S_Expressions.Enumeration_IO;
with Natools.S_Expressions.File_Readers;
with Natools.S_Expressions.File_Writers;
with Natools.S_Expressions.Interpreter_Loop;
with Natools.S_Expressions.Printers;
with Natools.Time_IO.RFC_3339;
with Natools.Web.Error_Pages;
with Natools.Web.Sites.Updates;

package body Natools.Web.Simple_Pages.Dynamic_Multipages is

   package Components is
      type Enum is (Error, Generator);
   end Components;

   package Component_IO is new S_Expressions.Enumeration_IO.Typed_IO
     (Components.Enum);

   type Page_Inserter (Path_Length : S_Expressions.Offset) is
     new Sites.Updates.Site_Update
   with record
      Path : S_Expressions.Atom (1 .. Path_Length);
      Page : Page_Ref;
   end record;


   function Build
     (Expression : in out S_Expressions.Lockable.Descriptor'Class;
      Defaults : in Default_Data;
      Root_Path : in S_Expressions.Atom;
      Path_Spec : in S_Expressions.Atom)
     return Page_Ref;

   procedure Build_And_Register
     (Builder : in out Sites.Site_Builder;
      Expression : in out S_Expressions.Lockable.Descriptor'Class;
      Defaults : in Default_Data;
      Root_Path : in S_Expressions.Atom;
      Path_Spec : in S_Expressions.Atom);

   procedure Build_And_Register
     (Site : in Sites.Site;
      Expression : in out S_Expressions.Lockable.Descriptor'Class;
      Defaults : in Default_Data;
      Root_Path : in S_Expressions.Atom;
      Path_Spec : in S_Expressions.Atom;
      Page_Web_Path : out S_Expressions.Atom_Refs.Immutable_Reference);

   function Key_Path (Path, Spec : S_Expressions.Atom)
     return S_Expressions.Atom;

   procedure Print
     (Descriptor : in Page_Descriptor;
      Printer : in out S_Expressions.Printers.Printer'Class;
      Wrapped : in Boolean);

   function Subsecond_Digits (Date : in Ada.Calendar.Time) return Natural;

   procedure Update
     (Defaults : in out Default_Data;
      Context : in Meaningless_Type;
      Name : in S_Expressions.Atom;
      Arguments : in out S_Expressions.Lockable.Descriptor'Class);

   overriding procedure Update
     (Self : in Page_Inserter;
      Site : in out Sites.Site);

   function Web_Path (Path, Spec : S_Expressions.Atom)
     return S_Expressions.Atom_Refs.Immutable_Reference;


   procedure Update_Defaults is new S_Expressions.Interpreter_Loop
     (Default_Data, Meaningless_Type, Update);



   ------------------------------
   -- Local Helper Subprograms --
   ------------------------------

   function Build
     (Expression : in out S_Expressions.Lockable.Descriptor'Class;
      Defaults : in Default_Data;
      Root_Path : in S_Expressions.Atom;
      Path_Spec : in S_Expressions.Atom)
     return Page_Ref
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

      return Page;
   end Build;


   procedure Build_And_Register
     (Builder : in out Sites.Site_Builder;
      Expression : in out S_Expressions.Lockable.Descriptor'Class;
      Defaults : in Default_Data;
      Root_Path : in S_Expressions.Atom;
      Path_Spec : in S_Expressions.Atom) is
   begin
      Register
        (Build (Expression, Defaults, Root_Path, Path_Spec),
         Builder, Key_Path (Root_Path, Path_Spec));
   end Build_And_Register;


   procedure Build_And_Register
     (Site : in Sites.Site;
      Expression : in out S_Expressions.Lockable.Descriptor'Class;
      Defaults : in Default_Data;
      Root_Path : in S_Expressions.Atom;
      Path_Spec : in S_Expressions.Atom;
      Page_Web_Path : out S_Expressions.Atom_Refs.Immutable_Reference)
   is
      Page : constant Page_Ref
        := Build (Expression, Defaults, Root_Path, Path_Spec);
      Path : constant S_Expressions.Atom := Key_Path (Root_Path, Path_Spec);
   begin
      declare
         Mutator : constant Data_Refs.Mutator := Page.Ref.Update;
      begin
         Page_Web_Path := Mutator.Web_Path;
         Mutator.Comment_List.Live_Load (Site, Mutator.Self, Mutator.Web_Path);
      end;

      Site.Queue_Update (Page_Inserter'
        (Path_Length => Path'Length,
         Path => Path,
         Page => Page));
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


   procedure Print
     (Descriptor : in Page_Descriptor;
      Printer : in out S_Expressions.Printers.Printer'Class;
      Wrapped : in Boolean) is
   begin
      if Wrapped then
         Printer.Open_List;
         Printer.Append_Atom (Descriptor.Name.Query);
      end if;

      if not Descriptor.Dates.Is_Empty then
         Printer.Open_List;
         Printer.Append_String ("dates");

         for Cursor in Descriptor.Dates.Iterate loop
            declare
               Element : constant Containers.Date
                 := Containers.Date_Maps.Element (Cursor);
            begin
               Printer.Open_List;
               Printer.Append_Atom (Containers.Date_Maps.Key (Cursor));
               Printer.Append_String
                 (Time_IO.RFC_3339.Image
                    (Date => Element.Time,
                     Time_Zone => Element.Offset,
                     Subsecond_Digits => Subsecond_Digits (Element.Time)));
               Printer.Close_List;
            end;
         end loop;

         Printer.Close_List;
      end if;

      if not Descriptor.Elements.Is_Empty then
         Printer.Open_List;
         Printer.Append_String ("elements");

         for Cursor in Descriptor.Elements.Iterate loop
            declare
               Element : S_Expressions.Caches.Cursor
                 := Containers.Expression_Maps.Element (Cursor);
            begin
               Printer.Open_List;
               Printer.Append_Atom (Containers.Expression_Maps.Key (Cursor));
               S_Expressions.Printers.Transfer
                 (Source => Element,
                  Target => Printer,
                  Check_Level => True);
               Printer.Close_List;
            end;
         end loop;

         Printer.Close_List;
      end if;

      Printer.Open_List;
      Printer.Append_String ("tags");
      Tags.Print (Descriptor.Tags, Printer);
      Printer.Close_List;

      if Wrapped then
         Printer.Close_List;
      end if;
   end Print;


   function Subsecond_Digits (Date : in Ada.Calendar.Time) return Natural is
      Result : Natural := 0;
      Subsecond : Duration := Ada.Calendar.Formatting.Sub_Second (Date);
      N : Natural;
   begin
      while Subsecond /= 0.0 loop
         Subsecond := Subsecond * 10;
         N := Natural (Subsecond);
         if Duration (N) > Subsecond then
            N := N - 1;
         end if;
         Subsecond := Subsecond - Duration (N);
         Result := Result + 1;
      end loop;

      return Result;
   end Subsecond_Digits;


   procedure Update
     (Defaults : in out Default_Data;
      Context : in Meaningless_Type;
      Name : in S_Expressions.Atom;
      Arguments : in out S_Expressions.Lockable.Descriptor'Class)
   is
      pragma Unreferenced (Context);
      use type S_Expressions.Events.Event;
      Event : S_Expressions.Events.Event;
   begin
      Update_Template :
      declare
         Known : Boolean;
      begin
         Set_Component (Defaults.Template, Name, Arguments, Known);

         if Known then
            return;
         end if;
      end Update_Template;

      case Component_IO.Value (Name, Components.Error) is
         when Components.Error =>
            Log (Severities.Error, "Unknown multipage default component """
              & S_Expressions.To_String (Name) & '"');

         when Components.Generator =>
            if Arguments.Current_Event = S_Expressions.Events.Add_Atom then
               Defaults.Generator_Path
                 := S_Expressions.Atom_Ref_Constructors.Create
                    (Arguments.Current_Atom);
               Arguments.Next (Event);

               if Event = S_Expressions.Events.Add_Atom then
                  declare
                     Name : constant S_Expressions.Atom
                       := Arguments.Current_Atom;
                     Cursor : constant Constructor_Maps.Cursor
                       := Constructor_DB.Find (Name);
                  begin
                     if not Constructor_Maps.Has_Element (Cursor) then
                        Log (Severities.Error,
                          "Unknown multipage constructor """
                          & S_Expressions.To_String (Name) & '"');
                     else
                        Arguments.Next;
                        declare
                           Constructor : constant Generator_Constructor
                             := Constructor_Maps.Element (Cursor);
                           Ptr : constant Generator_Refs.Data_Access
                             := new Generator'Class'(Constructor.all
                                                      (Arguments));
                        begin
                           Defaults.Generator := Generator_Refs.Create (Ptr);
                        end;
                     end if;
                  end;
               end if;
            end if;
      end case;
   end Update;


   overriding procedure Update
     (Self : in Page_Inserter;
      Site : in out Sites.Site) is
   begin
      Site.Insert (Self.Path, Self.Page);
      Site.Insert (Self.Page.Ref.Query.Data.Tags, Self.Page);
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
      Defaults : Default_Data
        := (File_Path => Object.File_Path,
            Web_Path => S_Expressions.Atom_Ref_Constructors.Create (Path),
            others => <>);

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

      if not Defaults.Generator.Is_Empty then
         declare
            use type S_Expressions.Atom;
            Post_Path : constant S_Expressions.Atom
              := Path & Defaults.Generator_Path.Query;
            Ref : constant Default_Data_Refs.Reference
              := Default_Data_Refs.Create (new Default_Data'(Defaults));
         begin
            Sites.Insert (Builder, Post_Path, Adder'(Ref => Ref));
         end;
      end if;
   end Load;


   procedure Register
     (Name : in String;
      Constructor : in Generator_Constructor)
   is
      Key : constant S_Expressions.Atom := S_Expressions.To_Atom (Name);
      Inserted : Boolean;
      Unused_Position : Constructor_Maps.Cursor;
   begin
      Constructor_DB := Constructor_Maps.Insert
        (Constructor_DB, Key, Constructor, Unused_Position, Inserted);

      if not Inserted then
         Log (Severities.Error,
           "Unable to register constructor """ & Name & """ in multipage");
      end if;
   end Register;


   overriding procedure Respond
     (Object : in out Adder;
      Exchange : in out Sites.Exchange;
      Extra_Path : in S_Expressions.Atom)
   is
      Descriptor : Page_Descriptor;
      Do_Post : Boolean;
      Defaults : constant Default_Data_Refs.Accessor := Object.Ref.Query;
      Page_Web_Path : S_Expressions.Atom_Refs.Immutable_Reference;
   begin
      if Defaults.Generator.Is_Empty then
         return;
      end if;

      Defaults.Generator.Update.Respond
        (Exchange, Extra_Path, Descriptor, Do_Post);

      if not Do_Post then
         return;
      end if;

      declare
         Exp : S_Expressions.Caches.Reference;
         Cursor : S_Expressions.Caches.Cursor;
         Writer : S_Expressions.File_Writers.Writer
           := S_Expressions.File_Writers.Open
              (S_Expressions.To_String (Object.Ref.Query.File_Path.Query));
      begin
         Print (Descriptor, Exp, False);

         Cursor := Exp.First;
         Exchange.Site.Set_Parameters (Writer);
         Writer.Open_List;
         Writer.Append_Atom (Descriptor.Name.Query);
         S_Expressions.Printers.Transfer (Cursor, Writer);
         Writer.Close_List;
         Writer.Newline;

         Cursor := Exp.First;
         Build_And_Register
           (Exchange.Site.all,
            Cursor,
            Object.Ref.Query,
            Object.Ref.Query.Web_Path.Query,
            Descriptor.Name.Query,
            Page_Web_Path);

         if not Exchange.Has_Response then
            Natools.Web.Error_Pages.See_Other
              (Exchange,
               Page_Web_Path.Query);
         end if;
      end;
   end Respond;

end Natools.Web.Simple_Pages.Dynamic_Multipages;
