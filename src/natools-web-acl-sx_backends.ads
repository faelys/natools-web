------------------------------------------------------------------------------
-- Copyright (c) 2017-2019, Natacha Porté                                   --
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
-- Natools.Web.ACL.Sx_Backends provides an extremely simple ACL backend     --
-- based on a S-expression file.                                            --
------------------------------------------------------------------------------

private with Natools.Constant_Indefinite_Ordered_Maps;
private with Natools.References;
private with Natools.Storage_Pools;

package Natools.Web.ACL.Sx_Backends is

   type Backend is new ACL.Backend with private;

   overriding procedure Authenticate
     (Self : in Backend;
      Exchange : in out Exchanges.Exchange);

   function Create
     (Arguments : in out S_Expressions.Lockable.Descriptor'Class)
     return ACL.Backend'Class;


   type Hash_Function is access function
     (Message : in S_Expressions.Atom)
     return S_Expressions.Atom;

   procedure Register
     (Id : in Character;
      Fn : in Hash_Function);

private

   subtype Hash_Id is Character;

   package Token_Maps is new Constant_Indefinite_Ordered_Maps
     (S_Expressions.Atom,      Containers.Identity,
      S_Expressions.Less_Than, Containers."=");

   type Backend is new ACL.Backend with record
      Map : Token_Maps.Constant_Map;
   end record;

   type Hash_Function_Array is array (Hash_Id range <>) of Hash_Function;

   package Hash_Function_Array_Refs is new References
     (Hash_Function_Array,
      Storage_Pools.Access_In_Default_Pool'Storage_Pool,
      Storage_Pools.Access_In_Default_Pool'Storage_Pool);

   Hash_Function_DB : Hash_Function_Array_Refs.Reference;

end Natools.Web.ACL.Sx_Backends;
