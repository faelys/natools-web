with Interfaces; use Interfaces;

package body Natools.Static_Maps.Web.Sites.Commands is

   P : constant array (0 .. 4) of Natural :=
     (1, 6, 9, 12, 14);

   T1 : constant array (0 .. 4) of Unsigned_8 :=
     (28, 19, 10, 27, 31);

   T2 : constant array (0 .. 4) of Unsigned_8 :=
     (34, 14, 23, 9, 3);

   G : constant array (0 .. 35) of Unsigned_8 :=
     (0, 0, 0, 11, 0, 4, 0, 0, 0, 11, 0, 0, 0, 4, 0, 10, 0, 1, 0, 0, 8, 0,
      6, 2, 5, 12, 0, 12, 3, 0, 7, 0, 7, 15, 0, 0);

   function Hash (S : String) return Natural is
      F : constant Natural := S'First - 1;
      L : constant Natural := S'Length;
      F1, F2 : Natural := 0;
      J : Natural;
   begin
      for K in P'Range loop
         exit when L < P (K);
         J  := Character'Pos (S (P (K) + F));
         F1 := (F1 + Natural (T1 (K)) * J) mod 36;
         F2 := (F2 + Natural (T2 (K)) * J) mod 36;
      end loop;
      return (Natural (G (F1)) + Natural (G (F2))) mod 17;
   end Hash;

end Natools.Static_Maps.Web.Sites.Commands;
