function drawCube ( origin, size )

x=([0 1 1 0 0 0;
    1 1 0 0 1 1;
    1 1 0 0 1 1;
    0 1 1 0 0 0]-0.5)*size+origin(1);
y=([0 0 1 1 0 0;
    0 1 1 0 0 0;
    0 1 1 0 1 1;
    0 0 1 1 1 1]-0.5)*size+origin(2);
z=([0 0 0 0 0 1;
    0 0 0 0 0 1;
    1 1 1 1 0 1;
    1 1 1 1 0 1]-0.5)*size+origin(3);
for i=1:6
    h=patch(x(:,i),y(:,i),z(:,i),'w');
    set(h,'edgecolor','b')
end 
axis([-5 5 -5 5 -5 5])