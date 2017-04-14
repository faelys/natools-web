------------------------------------------------------------------------------
-- Copyright (c) 2017, Natacha Porté                                        --
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

with Natools.S_Expressions.Atom_Buffers;
with Natools.S_Expressions.Atom_Ref_Constructors;
with Natools.S_Expressions.Interpreter_Loop;
with Natools.S_Expressions.Parsers;
with Natools.S_Expressions.Printers.Pretty;

package body Natools.Web.Comment_Cookies is

   Parameters : constant Natools.S_Expressions.Printers.Pretty.Parameters
     := (Width         => 0,
         Newline_At    => (others => (others => False)),
         Space_At      => (others => (others => False)),
         Tab_Stop      => <>,
         Indentation   => 0,
         Indent        => <>,
         Quoted        => S_Expressions.Printers.Pretty.No_Quoted,
         Token         => S_Expressions.Printers.Pretty.Extended_Token,
         Hex_Casing    => <>,
         Quoted_Escape => <>,
         Char_Encoding => <>,
         Fallback      => S_Expressions.Printers.Pretty.Verbatim,
         Newline       => S_Expressions.Printers.Pretty.LF);


   function Create (A : S_Expressions.Atom)
     return S_Expressions.Atom_Refs.Immutable_Reference
     renames S_Expressions.Atom_Ref_Constructors.Create;

   procedure Set_Atom
     (Info : in out Comment_Info;
      Context : in Meaningless_Type;
      Name : in S_Expressions.Atom;
      Arguments : in out S_Expressions.Lockable.Descriptor'Class);

   function To_Atom (Kind : in Atom_Kind) return S_Expressions.Atom;


   procedure Initialize is new S_Expressions.Interpreter_Loop
     (Comment_Info, Meaningless_Type, Set_Atom);


   ------------------------------
   -- Local Helper Subprograms --
   ------------------------------

   procedure Set_Atom
     (Info : in out Comment_Info;
      Context : in Meaningless_Type;
      Name : in S_Expressions.Atom;
      Arguments : in out S_Expressions.Lockable.Descriptor'Class)
   is
      pragma Unreferenced (Context);
      use type S_Expressions.Events.Event;
      Kind : Atom_Kind;
   begin
      if Arguments.Current_Event /= S_Expressions.Events.Add_Atom then
         return;
      end if;

      declare
         S_Name : constant String := S_Expressions.To_String (Name);
      begin
         Kind := Atom_Kind'Value (S_Name);
      exception
         when Constraint_Error =>
            Log (Severities.Error, "Unknown comment atom kind """
              & S_Name & '"');
      end;

      Info.Refs (Kind) := Create (Arguments.Current_Atom);
   end Set_Atom;


   function To_Atom (Kind : in Atom_Kind) return S_Expressions.Atom is
   begin
      case Kind is
         when Name =>
            return (Character'Pos ('N'), Character'Pos ('a'),
                    Character'Pos ('m'), Character'Pos ('e'));
         when Mail =>
            return (Character'Pos ('M'), Character'Pos ('a'),
                    Character'Pos ('i'), Character'Pos ('l'));
         when Link =>
            return (Character'Pos ('L'), Character'Pos ('n'),
                    Character'Pos ('n'), Character'Pos ('k'));
         when Filter =>
            return (Character'Pos ('F'), Character'Pos ('i'),
                    Character'Pos ('l'), Character'Pos ('t'),
                    Character'Pos ('e'), Character'Pos ('r'));
      end case;
   end To_Atom;



   -----------------------------------
   -- Comment Info Public Interface --
   -----------------------------------

   function Create
     (Name : in S_Expressions.Atom_Refs.Immutable_Reference;
      Mail : in S_Expressions.Atom_Refs.Immutable_Reference;
      Link : in S_Expressions.Atom_Refs.Immutable_Reference;
      Filter : in S_Expressions.Atom_Refs.Immutable_Reference)
     return Comment_Info is
   begin
      return (Refs =>
        (Comment_Cookies.Name => Name,
         Comment_Cookies.Mail => Mail,
         Comment_Cookies.Link => Link,
         Comment_Cookies.Filter => Filter));
   end Create;


   function Create
     (Expression : in out S_Expressions.Lockable.Descriptor'Class)
     return Comment_Info
   is
      use type S_Expressions.Events.Event;
      Result : Comment_Info := Null_Info;
      Event : S_Expressions.Events.Event;
   begin
      case Expression.Current_Event is
         when S_Expressions.Events.Add_Atom =>
            for Kind in Result.Refs'Range loop
               Result.Refs (Kind) := Create (Expression.Current_Atom);
               Expression.Next (Event);
               exit when Event /= S_Expressions.Events.Add_Atom;
            end loop;

         when S_Expressions.Events.Open_List =>
            Initialize (Expression, Result, Meaningless_Value);

         when S_Expressions.Events.Close_List
           | S_Expressions.Events.End_Of_Input
           | S_Expressions.Events.Error
         =>
            null;
      end case;

      return Result;
   end Create;


   function Named_Serialization (Info : in Comment_Info)
     return S_Expressions.Atom
   is
      Buffer : aliased S_Expressions.Atom_Buffers.Atom_Buffer;
      Printer : S_Expressions.Printers.Pretty.Stream_Printer (Buffer'Access);
   begin
      Printer.Set_Parameters (Parameters);
      Buffer.Preallocate (120);

      for Kind in Atom_Kind loop
         if not Info.Refs (Kind).Is_Empty then
            Printer.Open_List;
            Printer.Append_Atom (To_Atom (Kind));
            Printer.Append_Atom (Info.Refs (Kind).Query);
            Printer.Close_List;
         end if;
      end loop;

      return Buffer.Data;
   end Named_Serialization;


   function Positional_Serialization (Info : in Comment_Info)
     return S_Expressions.Atom
   is
      Buffer : aliased S_Expressions.Atom_Buffers.Atom_Buffer;
      Printer : S_Expressions.Printers.Pretty.Stream_Printer (Buffer'Access);
      Last : Atom_Kind;
   begin
      Printer.Set_Parameters (Parameters);
      Buffer.Preallocate (120);

      Last := Atom_Kind'Last;
      while Info.Refs (Last).Is_Empty loop
         if Last = Atom_Kind'First then
            return S_Expressions.Null_Atom;
         else
            Last := Atom_Kind'Pred (Last);
         end if;
      end loop;

      for Kind in Atom_Kind'First .. Last loop
         if Info.Refs (Kind).Is_Empty then
            Printer.Append_Atom (S_Expressions.Null_Atom);
         else
            Printer.Append_Atom (Info.Refs (Kind).Query);
         end if;
      end loop;

      return Buffer.Data;
   end Positional_Serialization;



   -------------------------------
   -- Codec DB Public Interface --
   -------------------------------

   function Decode
     (DB : in Codec_DB;
      Cookie : in String)
     return Comment_Info
   is
      Cursor : Decoder_Maps.Cursor;
   begin
      if Cookie'Length = 0 then
         return Null_Info;
      end if;

      Cursor := DB.Dec.Find (Cookie (Cookie'First));

      if not Decoder_Maps.Has_Element (Cursor) then
         return Null_Info;
      end if;

      declare
         Parser : S_Expressions.Parsers.Memory_Parser
           := S_Expressions.Parsers.Create
              (Decoder_Maps.Element (Cursor).all (Cookie));
      begin
         return Create (Parser);
      end;
   end Decode;


   function Encode
     (DB : in Codec_DB;
      Info : in Comment_Info)
     return String is
   begin
      if DB.Enc = null then
         raise Program_Error
           with "Comment_Cookie.Encode called before Set_Encoder";
      end if;

      case DB.Serialization is
         when Named =>
            return DB.Enc.all (Named_Serialization (Info));

         when Positional =>
            return DB.Enc.all (Positional_Serialization (Info));
      end case;
   end Encode;


   procedure Register
     (DB : in out Codec_DB;
      Key : in Character;
      Filter : in not null Decoder) is
   begin
      DB.Dec := Decoder_Maps.Include (DB.Dec, Key, Filter);
   end Register;


   procedure Set_Encoder
     (DB : in out Codec_DB;
      Filter : in not null Encoder;
      Serialization : in Serialization_Kind) is
   begin
      DB.Enc := Filter;
      DB.Serialization := Serialization;
   end Set_Encoder;

end Natools.Web.Comment_Cookies;
