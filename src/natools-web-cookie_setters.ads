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

------------------------------------------------------------------------------
-- Natools.Web.Cookie_Setters provides and end-point to set                 --
-- more or less abritrary cookies.                                          --
------------------------------------------------------------------------------

with Natools.Web.Sites;
with Natools.S_Expressions;

private with Natools.S_Expressions.Atom_Refs;
private with Natools.Web.Containers;

package Natools.Web.Cookie_Setters is

   type Setter is new Sites.Page with private;

   overriding procedure Respond
     (Object : in out Setter;
      Exchange : in out Sites.Exchange;
      Extra_Path : in S_Expressions.Atom);



   type Loader is new Sites.Page_Loader with private;

   overriding procedure Load
     (Object : in out Loader;
      Builder : in out Sites.Site_Builder;
      Path : in S_Expressions.Atom);

   function Create (File : in S_Expressions.Atom)
     return Sites.Page_Loader'Class;

private

   Default_Max_Age : constant := 10.0 * 365.0 * 86400.0;
   Default_Max_Length : constant := 50;

   type Setter is new Sites.Page with record
      Redirect_Target : S_Expressions.Atom_Refs.Immutable_Reference;
      Force_Name : S_Expressions.Atom_Refs.Immutable_Reference;
      Allowed_Names : Containers.Atom_Set;
      Path : S_Expressions.Atom_Refs.Immutable_Reference;
      Comment : S_Expressions.Atom_Refs.Immutable_Reference;
      Domain : S_Expressions.Atom_Refs.Immutable_Reference;
      Max_Age : Duration := Default_Max_Age;
      Secure : Boolean := False;
      HTTP_Only : Boolean := False;
      Max_Length : Positive := Default_Max_Length;
   end record;

   function Comment (Object : in Setter) return String
     is (if Object.Comment.Is_Empty
         then "" else S_Expressions.To_String (Object.Comment.Query));

   function Domain (Object : in Setter) return String
     is (if Object.Domain.Is_Empty
         then "" else S_Expressions.To_String (Object.Domain.Query));

   function Path (Object : in Setter) return String
     is (if Object.Path.Is_Empty
         then "/" else S_Expressions.To_String (Object.Path.Query));


   type Loader is new Sites.Page_Loader with record
      File_Name : S_Expressions.Atom_Refs.Immutable_Reference;
   end record;

end Natools.Web.Cookie_Setters;
