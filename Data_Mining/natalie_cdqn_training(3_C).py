import numpy as np
import random
from collections import deque
import tensorflow as tf
from tensorflow.keras.models import Model
from tensorflow.keras.layers import Input, Conv2D, Flatten, Dense, concatenate, Add
import tensorflow.keras.optimizers as optimizers
import os
from contextlib import redirect_stdout, redirect_stderr

# Parameters for the environment and training
BOARD_HEIGHT = 20
BOARD_WIDTH = 10
LINE_CLEAR_MULTIPLIER = 2
HEIGHT_MULTIPLIER = -2
ROW_INCENTIVE_MULTIPLIER = 1
GAME_OVER_REWARD = -2
ACTION_SPACE_SIZE = BOARD_WIDTH * 4  # 10 positions * 4 rotations
GAMMA = 0.99  # Discount factor for future rewards
LEARNING_RATE = 0.0001
BATCH_SIZE = 512
TARGET_UPDATE_FREQUENCY = 500
MEMORY_SIZE = 100000
EPSILON = 1.0  # Exploration rate
EPSILON_DECAY = 0.9999
EPSILON_MIN = 0.05
EPISODES = 10000
#TEST
LINE_CLEAR_MULTIPLIER = 10
HEIGHT_MULTIPLIER = -5
BUMPINESS_PENALTY = -5
FLATNESS_REWARD = 0.5
GAME_OVER_REWARD = -2000
HOLE_PENALTY = -5 

# Tetris Environment (Basic Version)
# (0,0) describes top left of board
class TetrisGame:
    def __init__(self):
        self.board = np.zeros((BOARD_HEIGHT, BOARD_WIDTH), dtype=int)
        self.pieces = {
            "I": [
                np.matrix([
                    [1],
                    [1],
                    [1],
                    [1]
                    ]),
                np.matrix([
                    [1,1,1,1]
                ])
            ], 
            "O": [
                np.matrix([
                    [1,1],
                    [1,1]
                    ]),
            ], 
            "T": [
                np.matrix([
                    [0,1,0],
                    [1,1,1],
                    ]),
                np.matrix([
                    [1,0],
                    [1,1],
                    [1,0],
                    ]),
                np.matrix([
                    [1,1,1],
                    [0,1,0],
                    ]),
                np.matrix([
                    [0,1],
                    [1,1],
                    [0,1],
                    ])
            ], 
            "S": [
                np.matrix([
                    [0,1,1],
                    [1,1,0],
                    ]),
                np.matrix([
                    [1,0],
                    [1,1],
                    [0,1],
                    ])
            ], 
            "Z": [
                np.matrix([
                    [1,1,0],
                    [0,1,1],
                    ]),
                np.matrix([
                    [0,1],
                    [1,1],
                    [1,0],
                    ])
            ], 
            "J": [
                np.matrix([
                    [1,1],
                    [1,0],
                    [1,0],
                    ]),
                np.matrix([
                    [1,1,1],
                    [0,0,1],
                    ]),
                np.matrix([
                    [0,1],
                    [0,1],
                    [1,1],
                    ]),
                np.matrix([
                    [1,0,0],
                    [1,1,1],
                    ])
            ], 
            "L": [
                np.matrix([
                    [1,0],
                    [1,0],
                    [1,1],
                    ]),
                np.matrix([
                    [1,1,1],
                    [1,0,0],
                    ]),
                np.matrix([
                    [1,1],
                    [0,1],
                    [0,1],
                    ]),
                np.matrix([
                    [0,0,1],
                    [1,1,1],
                    ])
            ]
        }
        self.current_piece = self.new_piece()
        self.next_piece_1 = self.new_piece()
        self.next_piece_2 = self.new_piece()



    def drop_piece(self, piece='l', column=0, rotation=0):

        piece = piece.upper()
        if piece not in self.pieces:
            print('Bad piece.')
            return -1

        if column >= BOARD_WIDTH or column < 0:
            print('Bad column.')
            return -1

        if rotation < 0 or rotation >= 4:
            print('Bad rotation.')
            return -1

        piece_encoding = self.pieces[piece]
        while len(piece_encoding) < rotation:
            rotation = rotation // 2
            
        piece_encoding = piece_encoding[rotation]
        piece_indices = piece_encoding.nonzero()
        piece_height = np.max(piece_indices[0]) + 1
        # first_row_to_check = BOARD_HEIGHT - piece_height
        first_row_to_check = 0
        max_row_to_check = BOARD_HEIGHT - piece_height
        piece_indices = np.transpose(piece_encoding)

        first_that_cannot_be_placed = first_row_to_check
        for row in range(first_row_to_check, max_row_to_check):
            if not self.can_be_placed(piece, row, column, rotation):
                first_that_cannot_be_placed = row
                break
        else:
            first_that_cannot_be_placed = row + 2

        if first_that_cannot_be_placed <= first_row_to_check:
            return -1 # Can't drop piece in location, lose game

        row = first_that_cannot_be_placed - 1 # Last that CAN be placed

        for i in range(piece_encoding.shape[0]):
            for j in range(piece_encoding.shape[1]):
                if piece_encoding[i,j] == 1:
                    self.board[row+i, column+j] = piece_encoding[i,j]

        removed_lines = 0
        return_row_index = row

        for row_index in range(BOARD_HEIGHT-1, -1, -1):
            row = self.board[row_index]
            while np.all(row == 1):
                removed_lines += 1
                for i in range(row_index, 0, -1):
                    self.board[i] = self.board[i-1]
                for element_index in range(len(self.board[0])):
                    self.board[0, element_index] = 0
                row = self.board[row_index]

        self.current_piece = self.next_piece_1
        self.next_piece_1 = self.next_piece_2
        self.next_piece_2 = self.new_piece()

        return removed_lines, return_row_index

    def can_be_dropped(self, piece, column, rotation):
        piece = piece.upper()
        if piece not in self.pieces:
            print('Bad piece.')
            return -1

        if column >= BOARD_WIDTH or column < 0:
            print('Bad column.')
            return -1

        if rotation < 0 or rotation >= 4:
            print('Bad rotation.')
            return -1

        piece_encoding = self.pieces[piece]
        while len(piece_encoding) < rotation:
            rotation = rotation // 2
            
        return self.can_be_placed(piece, 0, column, rotation)

    def can_be_placed(self, piece, row, column, rotation):

        piece = piece.upper()
        if piece not in self.pieces:
            return False

        piece_encoding = self.pieces[piece]
        while len(piece_encoding) <= rotation:
            rotation = rotation // 2
            
        piece_encoding = piece_encoding[rotation]

        for i in range(piece_encoding.shape[0]):
            for j in range(piece_encoding.shape[1]):
                # print(board[row+i, column+j])
                # print(piece_encoding[i, j])
                try:
                    if self.board[row+i, column+j] == 1 and piece_encoding[i, j] == 1:
                        # print(i, j)
                        return False
                except:
                    return False

        return True

    def new_piece(self):
        return random.choice(list(self.pieces.keys()))

    
    # def is_game_over(self):
    #     return False
    def is_game_over(self):
        for col in range(BOARD_WIDTH):
            for rotation in range(4):
                if self.can_be_dropped(self.current_piece, col, rotation):
                    return False
        return True

    def print(self, removed_lines):

        whole_board = ''
        for row in self.board:
            blocks = ''
            is_row_1 = True
            for block in row:

                yippee = ''
                if is_row_1:
                    if removed_lines >= 1:
                        yippee += '    YIPPEE'
                    is_row_1 = False

                if block == 1:
                    blocks += 'o '
                else:
                    blocks += '  '
            whole_board += '[ ' + blocks + ']' + yippee + '\n'

        print(whole_board)
    # def print(self):
    #         print(self.board)

    def get_column_heights(self, board=None):
        if board is None:
            board = self.board
        
        column_heights = []
        for column in board.T:
            column_height = 0
            block_indices = np.argwhere(column > 0)
            if len(block_indices) > 0:
                column_height = block_indices[0][0]
                column_height = BOARD_HEIGHT - column_height
            column_heights.append(column_height) 
        return column_heights       

    def get_game_state(self):
        return self.get_column_heights(), self.current_piece, self.next_piece_1, self.next_piece_2

    # def step(self, column, rotation):

    #     orig_board_indices = self.board.nonzero()
    #     # print(orig_board_indices)
    #     if len(orig_board_indices[0]) > 0:
    #         orig_board_height = BOARD_HEIGHT - np.min(orig_board_indices[0])
    #     else:
    #         orig_board_height = 0

    #     try:
    #         removed_lines, row = self.drop_piece(self.current_piece, column, rotation)
    #     except:
    #         removed_lines, row = -1, -1

    #     reward = removed_lines * LINE_CLEAR_MULTIPLIER if removed_lines >= 0 else -1

    #     if removed_lines == -1:
    #         reward += GAME_OVER_REWARD
    #         next_board_state, next_current_piece, next_piece_1, next_piece_2 = self.get_game_state()
    #         return next_board_state, next_current_piece, next_piece_1, next_piece_2, reward, True

    #     # get highest line
    #     new_board_indices = self.board.nonzero()
    #     print(new_board_indices)
    #     new_board_height = BOARD_HEIGHT - np.min(new_board_indices[0])
    #     print(np.min(new_board_indices)
    #     # incentivize placing on low rows
    #     reward += row * ROW_INCENTIVE_MULTIPLIER

    #     # print("eep")

    #     self.print()

    #     # print(new_board_height)
    #     # print(orig_board_height)

    #     reward += (new_board_height - orig_board_height) * HEIGHT_MULTIPLIER
    #     # reward += new_board_height * HEIGHT_MULTIPLIER

    #     # if board_height < HEIGHT_PREFERRED_MAX:
    #     #     next_board_state, next_current_piece, next_piece_1, next_piece_2 = self.get_game_state()
    #     #     return next_board_state, next_current_piece, next_piece_1, next_piece_2, reward, False

    #     next_board_state, next_current_piece, next_piece_1, next_piece_2 = self.get_game_state()

    #     # return next_board_state, next_current_piece, next_piece_1, next_piece_2, state - board_height, False
    #     return next_board_state, next_current_piece, next_piece_1, next_piece_2, reward, False

    #TEST


    def calculate_bumpiness(self):
        column_heights = [BOARD_HEIGHT - np.min(self.board[:, col].nonzero()[0]) if self.board[:, col].any() else 0
                        for col in range(BOARD_WIDTH)]
        bumpiness = sum(abs(column_heights[i] - column_heights[i + 1]) for i in range(len(column_heights) - 1))
        return bumpiness

    def calculate_flatness(self):
        column_heights = [BOARD_HEIGHT - np.min(self.board[:, col].nonzero()[0]) if self.board[:, col].any() else 0
                        for col in range(BOARD_WIDTH)]
        flatness = -np.std(column_heights)  # Negative standard deviation as a penalty.
        return flatness

    def calc_hole_penalty(self, board):

        holes = 0
        for col in range(board.shape[1]):  # Iterate through each column
            column = board[:, col]  # Extract the column
            block_found = False
            for cell in column:
                if cell == 1:
                    block_found = True  # Mark when a block is found
                elif block_found and cell == 0:
                    holes += 1  # Count empty spaces below a block
        
        # Return a scaled penalty (negative value for use in reward calculation)
        return holes * HOLE_PENALTY

    def step(self, column, rotation):
        """
        Executes a step in the Tetris game using the given column and rotation.
        
        Args:
            column (int): The column to drop the piece into.
            rotation (int): The rotation of the piece.

        Returns:
            next_board_state (np.ndarray): The board's state after the move.
            next_current_piece (int): The next piece in play.
            next_piece_1 (int): The next-next piece.
            next_piece_2 (int): The piece after next-next.
            reward (float): The calculated reward for this step.
            is_game_over (bool): Whether the game ended as a result of this move.
        """
        orig_board_indices = self.board.nonzero()
        orig_board_height = BOARD_HEIGHT - np.min(orig_board_indices[0]) if orig_board_indices[0].size > 0 else 0

        try:
            removed_lines, row = self.drop_piece(self.current_piece, column, rotation)
        except Exception as e:
            removed_lines, row = -1, -1

        if removed_lines == -1:
            reward = GAME_OVER_REWARD - 1
            next_state = self.get_game_state()
            #TEST
            #if (agent.epsilon < 1): print("eepers")
            return *next_state, reward, True


        #TEST
        if not any(self.can_be_dropped(self.current_piece, col, rot) for col in range(BOARD_WIDTH) for rot in range(4)):
            return *self.get_game_state(), GAME_OVER_REWARD, True
        
        new_board_indices = self.board.nonzero()
        new_board_height = BOARD_HEIGHT - np.min(new_board_indices[0]) if new_board_indices[0].size > 0 else 0

        reward = 0
        #TEST
        # Reward for clearing lines
        if removed_lines > 0:
            reward += (3 ** removed_lines - 1) * LINE_CLEAR_MULTIPLIER

        # Incentivize keeping the board low
        reward += (new_board_height - orig_board_height) * HEIGHT_MULTIPLIER

        # Reward/penalize based on the bumpiness of the board
        reward += self.calculate_bumpiness() * BUMPINESS_PENALTY

        # Encourage flat boards
        reward += self.calculate_flatness() * FLATNESS_REWARD
        # Existing reward calculation
        reward += removed_lines * LINE_CLEAR_MULTIPLIER if removed_lines > 0 else -1

        # Apply hole penalty
        hole_penalty = self.calc_hole_penalty(self.board)
        reward += hole_penalty

        # Game over penalty
        if removed_lines == -1:
            reward += GAME_OVER_REWARD
        next_state = self.get_game_state()
        if (agent.epsilon < 1): self.print(removed_lines)

        return *next_state, reward, False

    def reset(self):
        self.board = np.zeros((BOARD_HEIGHT, BOARD_WIDTH), dtype=int)
        self.current_piece = self.new_piece()
        self.next_piece_1 = self.new_piece()
        self.next_piece_2 = self.new_piece()

        return self.get_game_state()

    def get_valid_actions(self):

        valid_actions = np.full(BOARD_WIDTH * 4, True, dtype=bool)

        for action in range(len(valid_actions)):
            column = action // 4
            rotation = action % 4

            if rotation >= len(self.pieces[self.current_piece]):
                valid_actions[action] = False

            if not self.can_be_dropped(self.current_piece, column, rotation):
                valid_actions[action] = False

        return valid_actions

# Deep Q-Network Agent
class DQNAgent:
    def __init__(self, model=None, starting_epsilon=1.0):
        self.memory = deque(maxlen=MEMORY_SIZE)
        self.epsilon = EPSILON
        self.epsilon_min = EPSILON_MIN
        self.epsilon_decay = EPSILON_DECAY
        self.model = self.build_model()
        self.target_model = self.build_model()
        if model is not None:
            self.model.load_weights(model)
        self.update_target_model()
        self.replay_counter = 0

        self.epsilon = starting_epsilon
        if self.epsilon < self.epsilon_min:
            self.epsilon_min = self.epsilon

    def update_target_model(self):
        self.target_model.set_weights(self.model.get_weights())

    def build_model(self):
        # Input Layer: State size (board state flattened + additional piece info)
        board_input = Input(shape=(BOARD_WIDTH,), name='board_input')

        # Additional piece information (encoded into 21 bits)
        piece_info_input = Input(shape=(21,), name='piece_info_input')    

        # Combine board features with piece info
        combined = concatenate([board_input, piece_info_input])   

        # Fully Connected Layers
        fc1 = Dense(256, activation='relu')(combined)
        fc2 = Dense(128, activation='relu')(fc1)
        fc3 = Dense(64, activation='relu')(fc2)

        # Output Layer for Actions (BOARD_WIDTH * 4 rotations)
        action_output = Dense(ACTION_SPACE_SIZE, activation='linear', name='action_output')(fc3)

        # Create the Model
        model = Model(inputs=[board_input, piece_info_input], outputs=action_output)
        model.compile(optimizer=optimizers.Adam(learning_rate=LEARNING_RATE, clipnorm=10.0), loss='mse')
        return model

    def remember(self, state, action, reward, next_state, done):
        self.memory.append((state, action, reward, next_state, done))

    def act(self, state):
        board_state, piece_info, valid_actions = state

        if np.random.rand() <= self.epsilon:
            random_values = np.random.rand(ACTION_SPACE_SIZE)
            random_values[~valid_actions] = -np.inf
            random_index = np.argmax(random_values)
            column = random_index // 4
            rotation = random_index % 4
            return column, rotation, random_index

        # print(board_state.shape)  # Should be (batch_size, height, width, channels)
        # print(piece_info.shape)  # Should be (batch_size, 9)

        q_values = self.model.predict([board_state, piece_info])

        q_values[0][~valid_actions] = 0
        argmax_index = np.argmax(q_values[0])
        column = argmax_index // 4
        rotation = argmax_index % 4

        # print(f"HELLO: dropping at column {column}, rotation {rotation}, q_value {np.max(q_values[0])}")
        # print(q_values[0])


        return column, rotation, argmax_index

    def replay(self):
        if len(self.memory) < MEMORY_SIZE:
            return

        # Sample a minibatch of experiences from memory
        minibatch = random.sample(self.memory, BATCH_SIZE)

        # Separate components of the minibatch
        states = [x[0] for x in minibatch]
        board_states = [x[0] for x in states]
        piece_infos = [x[1] for x in states]
        valid_actions = [x[2] for x in states]

        next_states = [x[3] for x in minibatch]
        next_board_states = [x[0] for x in next_states]
        next_piece_infos = [x[1] for x in next_states]
        next_valid_actions = [x[2] for x in next_states]

        rewards = tf.convert_to_tensor([x[2] for x in minibatch], dtype=tf.float32)
        actions = tf.convert_to_tensor([x[1] for x in minibatch], dtype=tf.int32)
        dones = tf.convert_to_tensor([x[4] for x in minibatch], dtype=tf.float32)

        # print(f"Rewards: {rewards}")
        # print(f"Actions: {actions}")
        # print(f"Dones: {dones}")

        # Convert board and piece info to tensors for batch processing
        board_states = tf.convert_to_tensor(np.vstack(board_states), dtype=tf.float32)
        piece_infos = tf.convert_to_tensor(np.vstack(piece_infos), dtype=tf.float32)
        next_board_states = tf.convert_to_tensor(np.vstack(next_board_states), dtype=tf.float32)
        next_piece_infos = tf.convert_to_tensor(np.vstack(next_piece_infos), dtype=tf.float32)

        # Predict Q-values for current and next states
        q_value_cur = self.model([board_states, piece_infos])
        q_value_next = self.target_model([next_board_states, next_piece_infos])

        # Mask invalid actions (if applicable)
        valid_action_mask = tf.convert_to_tensor(valid_actions, dtype=tf.bool)
        next_valid_action_mask = tf.convert_to_tensor(next_valid_actions, dtype=tf.bool)
        q_value_cur = tf.where(valid_action_mask, q_value_cur, tf.zeros_like(q_value_cur))
        q_value_next = tf.where(next_valid_action_mask, q_value_next, tf.zeros_like(q_value_next))

        # Calculate target Q-values using Bellman equation
        target_q_values = rewards + GAMMA * tf.reduce_max(q_value_next, axis=1) * (1 - dones)

        # print(q_value_cur)
        # print(target_q_values)
        # input('')

        # Update Q-values only for actions taken
        indices = tf.range(BATCH_SIZE, dtype=tf.int32)
        action_indices = tf.stack([indices, actions], axis=1)
        q_value_cur = tf.tensor_scatter_nd_update(q_value_cur, action_indices, target_q_values)

        # Log predicted and target Q-values for debugging
        # print("Predicted Q-values:", q_value_cur.numpy())
        # print("Target Q-values:", target_q_values.numpy())

        # Train the model on the updated Q-values
        history = self.model.fit(
            x=[board_states, piece_infos],
            y=q_value_cur,
            batch_size=BATCH_SIZE,
            epochs=1,
            verbose=0  # Prevent direct console output
        )

        # Log the loss for this training step
        loss = history.history['loss'][0]
        print(f"Replay Loss: {loss}")

        # Adjust exploration rate (epsilon)
        #print("eep")
        if self.epsilon > self.epsilon_min:
            self.epsilon *= self.epsilon_decay

        # Periodically update the target network
        self.replay_counter += 1
        if self.replay_counter >= TARGET_UPDATE_FREQUENCY:
            # print("EEEP\n\n\n\n\n\n\n\n\n")
            self.update_target_model()
            self.model.save('tetris_models/model.keras')
            self.replay_counter = 0

    def encode_piece_info(self, current_piece, next_piece_1, next_piece_2):
        # Encode the current and next pieces into a 9-bit representation
        current_piece = current_piece.upper()
        next_piece_1 = next_piece_1.upper()
        next_piece_2 = next_piece_2.upper()
        piece_encoding = {
            'I': [1, 0, 0, 0, 0, 0, 0],
            'O': [0, 1, 0, 0, 0, 0, 0],
            'T': [0, 0, 1, 0, 0, 0, 0],
            'S': [0, 0, 0, 1, 0, 0, 0],
            'Z': [0, 0, 0, 0, 1, 0, 0],
            'J': [0, 0, 0, 0, 0, 1, 0],
            'L': [0, 0, 0, 0, 0, 0, 1]
        }
        return piece_encoding[current_piece] + piece_encoding[next_piece_1] + piece_encoding[next_piece_2]


env = TetrisGame()

#agent = DQNAgent(starting_epsilon=0.0, model='tetris_models/model.keras')
#agent = DQNAgent(starting_epsilon=0.3859113465007776, model='tetris_models/model.keras')
# agent = DQNAgent(starting_epsilon=0.3)
agent = DQNAgent()



for episode in range(EPISODES):
    state = env.reset()

    total_reward = 0
    while True:

        board_state, current_piece, next_piece_1, next_piece_2 = state

        # Convert board state and piece information into appropriate inputs
        board_state = np.expand_dims(board_state, axis=(0, -1))  # Shape (1, BOARD_WIDTH, 1)
        piece_info = np.array([agent.encode_piece_info(current_piece, next_piece_1, next_piece_2)])  # Shape (1, 21)

        valid_actions = env.get_valid_actions()

        # get next action
        column, rotation, action = agent.act((board_state, piece_info, valid_actions))

        # take step, get next state
        next_board_state, next_current_piece, next_piece_1, next_piece_2, reward, done = env.step(column, rotation)
        total_reward += reward
        
        cur_state = (board_state, piece_info, valid_actions)
        next_board_state = np.expand_dims(next_board_state, axis=(0, -1))
        next_piece_info = np.array([agent.encode_piece_info(next_current_piece, next_piece_1, next_piece_2)])
        next_valid_actions = env.get_valid_actions()
        next_state = (next_board_state, next_piece_info, next_valid_actions)

        agent.memory.append((cur_state, action, reward, next_state, done))

        board_state, current_piece, next_piece_1, next_piece_2 = next_board_state, next_current_piece, next_piece_1, next_piece_2

        if done:
            break

        with open(os.devnull, 'w') as f:
            with redirect_stdout(f), redirect_stderr(f):
                agent.replay()

        if (agent.epsilon < 1): print(f"Episode {episode + 1}/{EPISODES}, Total Reward: {total_reward}, Epsilon: {agent.epsilon}")
        total_reward = 0
