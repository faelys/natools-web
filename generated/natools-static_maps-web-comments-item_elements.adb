with Interfaces; use Interfaces;

package body Natools.Static_Maps.Web.Comments.Item_Elements is

   P : constant array (0 .. 1) of Natural :=
     (1, 5);

   T1 : constant array (0 .. 1) of Unsigned_8 :=
     (13, 6);

   T2 : constant array (0 .. 1) of Unsigned_8 :=
     (16, 3);

   G : constant array (0 .. 23) of Unsigned_8 :=
     (0, 2, 0, 0, 0, 0, 0, 6, 0, 0, 0, 5, 4, 0, 1, 6, 0, 2, 0, 0, 8, 0, 0,
      9);

   function Hash (S : String) return Natural is
      F : constant Natural := S'First - 1;
      L : constant Natural := S'Length;
      F1, F2 : Natural := 0;
      J : Natural;
   begin
      for K in P'Range loop
         exit when L < P (K);
         J  := Character'Pos (S (P (K) + F));
         F1 := (F1 + Natural (T1 (K)) * J) mod 24;
         F2 := (F2 + Natural (T2 (K)) * J) mod 24;
      end loop;
      return (Natural (G (F1)) + Natural (G (F2))) mod 11;
   end Hash;

end Natools.Static_Maps.Web.Comments.Item_Elements;
