with Interfaces; use Interfaces;

package body Natools.Static_Maps.Web.Comments.Item_Actions is

   P : constant array (0 .. 1) of Natural :=
     (2, 3);

   T1 : constant array (0 .. 1) of Unsigned_8 :=
     (26, 29);

   T2 : constant array (0 .. 1) of Unsigned_8 :=
     (9, 3);

   G : constant array (0 .. 33) of Unsigned_8 :=
     (0, 0, 0, 0, 15, 0, 0, 0, 0, 0, 12, 7, 1, 0, 0, 9, 8, 0, 2, 0, 11, 0,
      11, 0, 0, 13, 7, 0, 13, 0, 8, 0, 12, 14);

   function Hash (S : String) return Natural is
      F : constant Natural := S'First - 1;
      L : constant Natural := S'Length;
      F1, F2 : Natural := 0;
      J : Natural;
   begin
      for K in P'Range loop
         exit when L < P (K);
         J  := Character'Pos (S (P (K) + F));
         F1 := (F1 + Natural (T1 (K)) * J) mod 34;
         F2 := (F2 + Natural (T2 (K)) * J) mod 34;
      end loop;
      return (Natural (G (F1)) + Natural (G (F2))) mod 16;
   end Hash;

end Natools.Static_Maps.Web.Comments.Item_Actions;
