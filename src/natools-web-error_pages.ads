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

------------------------------------------------------------------------------
-- Natools.Web.Error_Pages provides procedures to build error pages from    --
-- site-wide templates, first trying "error-page-XXX" where XXX is the      --
-- HTTP error code, then "error-page", then a hardcoded fallback.           --
------------------------------------------------------------------------------

with Natools.S_Expressions;
with Natools.Web.Exchanges;
with Natools.Web.Sites;

private with Natools.S_Expressions.Atom_Refs;

package Natools.Web.Error_Pages is

   procedure Method_Not_Allowed
     (Exchange : in out Sites.Exchange;
      Allow : in Exchanges.Method_Set);

   procedure Not_Found (Exchange : in out Sites.Exchange);

   procedure Permanent_Redirect
     (Exchange : in out Sites.Exchange;
      Location : in S_Expressions.Atom);

   procedure See_Other
     (Exchange : in out Sites.Exchange;
      Location : in S_Expressions.Atom);



   procedure Check_Method
     (Exchange : in out Sites.Exchange;
      Allowed_Set : in Exchanges.Method_Set;
      Allowed : out Boolean);
   procedure Check_Method
     (Exchange : in out Sites.Exchange;
      Allowed_Methods : in Exchanges.Method_Array;
      Allowed : out Boolean);
   procedure Check_Method
     (Exchange : in out Sites.Exchange;
      Allowed_Method : in Exchanges.Known_Method;
      Allowed : out Boolean);
      --  Check whether the method of Exchange is allowed, and if not
      --  call Method_Not_Allowed.


private

   type Error_Context is record
      Location : S_Expressions.Atom_Refs.Immutable_Reference;
      Code : S_Expressions.Atom (1 .. 3);
   end record;

   procedure Main_Render
     (Exchange : in out Sites.Exchange;
      Context : in Error_Context);

end Natools.Web.Error_Pages;
