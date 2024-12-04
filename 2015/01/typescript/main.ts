export function add(a: number, b: number): number {
  return a + b;
}

class Elevator {
  floor: number

  constructor() {
    this.floor = 0;
  }

  getCurrentFloor(): number {
    return this.floor;
  }

  goUp() {
    this.floor += 1;
  }

  goDown() {
    this.floor -= 1;
  }
}

class Reader {
  elevator: Elevator
  fileName: string

  constructor(fileName: string, elevator: Elevator) {
    this.fileName = fileName;
    this.elevator = elevator;
  }

  async process() {
    const text = await Deno.readTextFile(this.fileName);
    for (const char of text) {
      switch (char) {
      case "(":
        this.elevator.goUp();
        break;
      case ")":
        this.elevator.goDown();
        break;
      default:
        throw new Error(`Unrecognized character: ${char}`);
      }
    }
  }

}

async function solve1(): Promise<number> {
  const elevator = new Elevator();
  const reader = new Reader("input", elevator);
  await reader.process();
  return elevator.getCurrentFloor();
}

// Learn more at https://deno.land/manual/examples/module_metadata#concepts
if (import.meta.main) {
  console.log(`Solution 1: ${await solve1()}`);
}
