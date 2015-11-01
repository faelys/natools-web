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

with Natools.S_Expressions.Atom_Ref_Constructors;
with Natools.Web.Error_Pages;
with Natools.Web.Exchanges;
with Natools.Web.Sites.Updates;

package body Natools.Web.Reload_Pages is

   overriding procedure Respond
     (Object : in out Reload_Page;
      Exchange : in out Sites.Exchange;
      Extra_Path : in S_Expressions.Atom) is
   begin
      if Extra_Path'Length > 0 then
         return;
      end if;

      Check_Method :
      declare
         use Exchanges;
         Allowed : Boolean;
      begin
         Error_Pages.Check_Method (Exchange, POST, Allowed);

         if not Allowed then
            return;
         end if;
      end Check_Method;

      Sites.Updates.Reload (Exchange.Site.all);
      Error_Pages.See_Other (Exchange, Object.Target.Query);
   end Respond;



   overriding procedure Load
     (Object : in out Loader;
      Builder : in out Sites.Site_Builder;
      Path : in S_Expressions.Atom) is
   begin
      Sites.Insert (Builder, Path, Reload_Page'(Target => Object.Target));
   end Load;



   function Create (File : in S_Expressions.Atom)
     return Sites.Page_Loader'Class is
   begin
      return Loader'
        (Target => S_Expressions.Atom_Ref_Constructors.Create (File));
   end Create;

end Natools.Web.Reload_Pages;
