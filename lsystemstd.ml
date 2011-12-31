(* Primary Author: Timothy Sun (ts2578) *)

(* I'm going to say that most of the Java code's mine. :P -Tim *)

(* Standard java functions. *)
let std_main = "	public static void main(String[] args){
		JFrame j = new JFrame();
		CLASSNAME cn = new CLASSNAME();
		JScrollPane jsp = new JScrollPane(cn.jta);
		jsp.setPreferredSize(new Dimension(DEFAULT+2,100));
		j.add(cn, BorderLayout.CENTER);
		j.add(jsp, BorderLayout.PAGE_END);
		j.pack();
		j.setTitle(\"L-System: CLASSNAME\");
		j.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
		j.setVisible(true);		
	}
"

let std_render_signature = "	public void execute(){\n"
		
(* Standard Turtle functions for drawing. *)
let std_turtle1 =
"import java.awt.*;
import javax.swing.*;
import java.util.*;
import java.io.*;
import java.awt.image.BufferedImage;
class Turtle extends JPanel {
	public static final int EMPTY = -1;
	public static final int FORWARD = 0;
	public static final int TURN = 1;
	public static final int DOWN = 2;
	public static final int UP = 3;
	public static final int SETX = 4;
	public static final int SETY = 5;
	public static boolean testing = "

let std_turtle2 = "
	public static int DEFAULT = testing ? 100 : 400;
	private double x = 0;
	private double y = 0;
	private int height;
	private int width;
	public JTextArea jta;
	private double angle;
	private boolean down;
	private BufferedImage bi;
	private ArrayList<double[]> lines;
	public HashMap<String, Function> functions;
	public Turtle(){
		this(DEFAULT+2,DEFAULT+2,0);
	}
	public Turtle(int w, int h, double angle){
		setPreferredSize(new Dimension(w,h));
		height = h;
		width = w;
		this.angle = angle;
		this.down = true;
		jta = new JTextArea(5,20);
		jta.setEditable(false);
		setDoubleBuffered(true);
		lines = new ArrayList<double[]>();
		functions = new HashMap<String, Function>();
	}
	public class Function {
		public Function(String name){
			functions.put(name, this);
			terms = new HashMap<String, Command>();
			prods = new HashMap<String, String[]>();
			terms.put(\"f\", new Command(FORWARD,1));
			terms.put(\"l\", new Command(TURN,-90));
			terms.put(\"r\", new Command(TURN,90));
		}
		public void addTerminal(String symbol, Command command){
			terms.put(symbol, command);
		}
		public void addProduction(String symbol, String expansion){
			prods.put(symbol, expansion.split(\",\"));
		}
		public boolean hasTerminal(String symbol){
			return terms.containsKey(symbol);
		}
		public boolean hasProduction(String symbol){
			return prods.containsKey(symbol);
		}
		public Command getTerminal(String symbol){
			return terms.get(symbol);
		}
		public String[] getProduction(String symbol){
			return prods.get(symbol);
		}
		HashMap<String, Command> terms;
		HashMap<String, String[]> prods;
	}
	public class Command {
		public Command(){
			this(EMPTY);
		}
		public Command(int command){
			this(command, 0);
		}
		public Command(int command, int param){
			this.command = command;
			this.param = param;
		}
		int command;
		double param;
	}
	public void turtle(Command c){
		switch (c.command){
			case FORWARD: forward(c.param); break;
			case TURN: turn(c.param); break;
			case DOWN: down(); break;
			case UP: up(); break;
			case SETX: setX(c.param); break;
			case SETY: setY(c.param); break;
			default: break;
		}
	}
	public double[] getDim(){
		double minx = Double.MAX_VALUE, miny = Double.MAX_VALUE;
		for (double[] line : lines){
			if (minx > line[0] || minx > line[2])
				minx = Math.min(line[0],line[2]);
			if (miny > line[1] || miny > line[3])
				miny = Math.min(line[1],line[3]);
		}
		for (double[] line : lines){
			line[0] -= minx;
			line[1] -= miny;
			line[2] -= minx;
			line[3] -= miny;
		}
		double maxx = Double.MIN_VALUE, maxy = Double.MIN_VALUE;
		for (double[] line : lines){
			if (maxx < line[0] || maxx < line[2])
				maxx = Math.max(line[0],line[2]);
			if (maxy < line[1] || maxy < line[3])
				maxy = Math.max(line[1],line[3]);
		}
		return new double[]{maxx+1, maxy+1};
	}
	public void scale(double factor){
		double[] dim = getDim();
		double trueScale = factor*Math.min(width/dim[0],height/dim[1]);
		bi = new BufferedImage(width+1, height+1, BufferedImage.TYPE_INT_RGB);
		Graphics g = bi.getGraphics();
		g.setColor(Color.WHITE);
		g.fillRect(0, 0, width, height);
		g.setColor(Color.BLACK);
		for (double[] line : lines)
			g.drawLine((int)(line[0]*trueScale), (int)(line[1]*trueScale), (int)(line[2]*trueScale), (int)(line[3]*trueScale));
		if (testing){
			try {
				String output = \"\";
				for (int x = 0; x < width; x++){
					for (int y = 0; y < height; y++)
						output += bi.getRGB(x,y) == -1 ? \"1\" : \"0\";
					output += \"\\n\";
				}
				PrintWriter pw = new PrintWriter(new File(\""

let std_turtle3 =
".txt\"));
				pw.write(output);
				pw.close();
			}
			catch (Exception e){ e.printStackTrace(); }
			System.exit(1);
		}
	}
	public void paintComponent(Graphics g){
		if (height != getHeight() || width != getWidth()){
			height = getHeight();
			width = getWidth();
			scale(1);
		}
		super.paintComponent(g);
		g.drawImage(bi, 1, 1, Color.WHITE, this);
	}
	public void draw(String name, int depth){
		draw(functions.get(name), depth, \"lambda\");
	}
	public void draw(Function f, int depth, String symbol){
		if (depth == -1){
			if (f.hasTerminal(symbol))
				turtle(f.getTerminal(symbol));
		}
		else {
			String[] production = f.getProduction(symbol);
			for (String term : production){
				if (f.hasProduction(term))
					draw(f, depth-1, term);
				else if (f.hasTerminal(term))
					turtle(f.getTerminal(term));
			}
		}
	}
	public void down(){
		down = true;
	}
	public void up(){
		down = false;
	}
	public void forward(double t){
		t = t * 10;
		double nx = x + Math.cos(angle)*t;
		double ny = y + Math.sin(angle)*t;
		if (down)
			lines.add(new double[]{x, y, nx, ny});
		x = nx;
		y = ny;
	}
	public void turn(double deg){
		angle += deg*Math.PI/180.0;
	}		
	public void setX(double x){
		this.x = x;
	}
	public void setY(double y){
		this.y = y;
	}
	public void print(String args){
		jta.append(args);
		if (testing) System.out.println(args);
	}
	public void print(int args){
		print(args+\"\");
	}
	public void print(double args){
		print(args+\"\");
	}
	public void print(boolean args){
		print(args+\"\");
	}
}
"

(* A string list of reserved function names in the standard library. *)
let func_names = ["print"; "Turtle"; "down"; "up"; "forward"; "turn"; "paintComponent"; "resetPosition"; "setX"; "setY"]
let std_symbols = ["r"; "l"; "f"] (*Reserved symbols in draw functions*)
let std_lfunc = ["down"; "up"; "turn"; "forward"; "setX"; "setY"] (*Standard drawing functions callable from draw/compute functions*)
