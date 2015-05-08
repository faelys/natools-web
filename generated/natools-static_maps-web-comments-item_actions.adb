with Interfaces; use Interfaces;

package body Natools.Static_Maps.Web.Comments.Item_Actions is

   P : constant array (0 .. 0) of Natural :=
     (0 .. 0 => 3);

   T1 : constant array (0 .. 0) of Unsigned_8 :=
     (0 .. 0 => 9);

   T2 : constant array (0 .. 0) of Unsigned_8 :=
     (0 .. 0 => 13);

   G : constant array (0 .. 26) of Unsigned_8 :=
     (0, 8, 0, 0, 1, 0, 0, 0, 0, 0, 12, 0, 0, 2, 0, 4, 3, 9, 9, 7, 0, 0, 10,
      2, 6, 0, 0);

   function Hash (S : String) return Natural is
      F : constant Natural := S'First - 1;
      L : constant Natural := S'Length;
      F1, F2 : Natural := 0;
      J : Natural;
   begin
      for K in P'Range loop
         exit when L < P (K);
         J  := Character'Pos (S (P (K) + F));
         F1 := (F1 + Natural (T1 (K)) * J) mod 27;
         F2 := (F2 + Natural (T2 (K)) * J) mod 27;
      end loop;
      return (Natural (G (F1)) + Natural (G (F2))) mod 13;
   end Hash;

end Natools.Static_Maps.Web.Comments.Item_Actions;
