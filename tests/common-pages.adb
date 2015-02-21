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



package body Common.Pages is

   procedure Append_Line
     (Exchange : in out Sites.Exchange;
      Line : in String);


   ------------------------------
   -- Local Helper Subprograms --
   ------------------------------

   procedure Append_Line
     (Exchange : in out Sites.Exchange;
      Line : in String)
   is
      use type S_Expressions.Atom;
   begin
      Exchange.Append (S_Expressions.To_Atom (Line) & (1 => 10));
   end Append_Line;



   ----------------------
   -- Public Interface --
   ----------------------

   function Create (File : in S_Expressions.Atom)
     return Sites.Page_Loader'Class
   is
      pragma Unreferenced (File);
   begin
      return Loader'(null record);
   end Create;


   overriding procedure Load
     (Object : in out Loader;
      Builder : in out Sites.Site_Builder;
      Path : in S_Expressions.Atom)
   is
      pragma Unreferenced (Object);
   begin
      Sites.Insert (Builder, Path, Page'(null record));
   end Load;


   procedure Queue_Sleep (Site : in Sites.Site; Amount : in Duration) is
   begin
      Site.Queue_Update (Sleep_Update'(Amount => Amount));
   end Queue_Sleep;


   overriding procedure Respond
     (Object : in out Page;
      Exchange : in out Sites.Exchange;
      Extra_Path : in S_Expressions.Atom)
   is
      pragma Unreferenced (Object);
      use type S_Expressions.Atom;
   begin
      Exchange.Set_MIME_Type (S_Expressions.To_Atom ("text/plain"));

      Append_Line (Exchange, "Extra_Path: """
        & S_Expressions.To_String (Extra_Path) & '"');
      Append_Line (Exchange, "Version:" & Natural'Image (Counter.Get_Value));

      declare
         procedure Process (Name, Value : in String);

         Prefix_Sent : Boolean := False;

         procedure Process (Name, Value : in String) is
         begin
            if not Prefix_Sent then
               Append_Line (Exchange, "Parameters:");
               Prefix_Sent := True;
            end if;

            Append_Line (Exchange, "   " & Name & ": """ & Value & '"');
         end Process;
      begin
         Exchange.Iterate_Parameters (Process'Access);
      end;

      if Exchange.Parameter ("reload") /= "" then
         Sites.Updates.Reload (Exchange.Site.all);
         Append_Line (Exchange, "Reload enqeued.");
      end if;

      declare
         Sleep_Amount : Duration;
      begin
         Sleep_Amount := Duration'Value (Exchange.Parameter ("sleep_update"));
         Queue_Sleep (Exchange.Site.all, Sleep_Amount);
         Append_Line (Exchange, "Queue sleep for: "
           & Duration'Image (Sleep_Amount));
      exception
         when Constraint_Error => null;
      end;

      declare
         Version : Natural;
      begin
         Version := Natural'Value (Exchange.Parameter ("wait_version"));
         Counter.Wait_Version (Version);
         Append_Line (Exchange, "Waited version:" & Natural'Image (Version));
      exception
         when Constraint_Error => null;
      end;
   end Respond;


   overriding procedure Update
     (Self : in Sleep_Update;
      Site : in out Sites.Site)
   is
      pragma Unreferenced (Site);
   begin
      delay Self.Amount;
   end Update;

end Common.Pages;
