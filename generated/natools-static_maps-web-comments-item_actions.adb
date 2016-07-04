with Interfaces; use Interfaces;

package body Natools.Static_Maps.Web.Comments.Item_Actions is

   P : constant array (0 .. 2) of Natural :=
     (2, 3, 7);

   T1 : constant array (0 .. 2) of Unsigned_8 :=
     (38, 19, 3);

   T2 : constant array (0 .. 2) of Unsigned_8 :=
     (3, 37, 38);

   G : constant array (0 .. 38) of Unsigned_8 :=
     (15, 0, 16, 0, 4, 0, 14, 0, 6, 6, 8, 0, 0, 16, 0, 0, 0, 0, 0, 0, 0, 7,
      1, 0, 3, 0, 0, 7, 7, 5, 10, 12, 8, 0, 0, 0, 0, 11, 0);

   function Hash (S : String) return Natural is
      F : constant Natural := S'First - 1;
      L : constant Natural := S'Length;
      F1, F2 : Natural := 0;
      J : Natural;
   begin
      for K in P'Range loop
         exit when L < P (K);
         J  := Character'Pos (S (P (K) + F));
         F1 := (F1 + Natural (T1 (K)) * J) mod 39;
         F2 := (F2 + Natural (T2 (K)) * J) mod 39;
      end loop;
      return (Natural (G (F1)) + Natural (G (F2))) mod 19;
   end Hash;

end Natools.Static_Maps.Web.Comments.Item_Actions;
