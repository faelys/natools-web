with Interfaces; use Interfaces;

package body Natools.Static_Maps.Web.Comments.List_Elements is

   P : constant array (0 .. 1) of Natural :=
     (1, 3);

   T1 : constant array (0 .. 1) of Unsigned_8 :=
     (10, 2);

   T2 : constant array (0 .. 1) of Unsigned_8 :=
     (2, 17);

   G : constant array (0 .. 17) of Unsigned_8 :=
     (0, 3, 0, 4, 2, 0, 0, 0, 0, 0, 0, 0, 4, 0, 5, 0, 1, 2);

   function Hash (S : String) return Natural is
      F : constant Natural := S'First - 1;
      L : constant Natural := S'Length;
      F1, F2 : Natural := 0;
      J : Natural;
   begin
      for K in P'Range loop
         exit when L < P (K);
         J  := Character'Pos (S (P (K) + F));
         F1 := (F1 + Natural (T1 (K)) * J) mod 18;
         F2 := (F2 + Natural (T2 (K)) * J) mod 18;
      end loop;
      return (Natural (G (F1)) + Natural (G (F2))) mod 8;
   end Hash;

end Natools.Static_Maps.Web.Comments.List_Elements;
