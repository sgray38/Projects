import tensorflow as tf
import numpy as np
import gym
import math
from PIL import Image
import pygame, sys
from pygame.locals import *
from tensorflow import keras
from collections import deque
import random

env = gym.make('MountainCar-v0')

input_shape = env.observation_space.shape[0]
num_actions = env.action_space.n

value_network = tf.keras.models.Sequential([
    tf.keras.layers.Input(shape=(input_shape,)),
    tf.keras.layers.Dense(32, activation='relu'),
    tf.keras.layers.Dense(32, activation='relu'),
    tf.keras.layers.Dense(num_actions)
])

# set up optimizer, loss fnc
optimizer = tf.keras.optimizers.Adam(learning_rate=0.001)
loss_fn = tf.keras.losses.MeanSquaredError(reduction='sum_over_batch_size', name='mean_squared_error')
# value_network = tf.keras.models.load_model('keras')

num_episodes = 1000
epsilon = 1
gamma = 0.9
state = env.reset()
batch = 200
replay = deque(maxlen=2000)
epoch = 0
alpha = 0.1

for episode in range(num_episodes):
    state = env.reset()

    while True:

        if isinstance(state, tuple):
            state = state[0]

        value_function = value_network.predict(np.array([state]), verbose=0)[0]

        if np.random.rand() > epsilon:
            action = np.argmax(value_function)

        else:
            action = np.random.choice(num_actions)

        next_state, reward, done, _, _ = env.step(action)
        
        done = 1 if done else 0

        replay.append((state, action, reward, next_state, done))

        state = next_state
        # print(state)

        if done:
            break

        if len(replay) > batch:
            with tf.GradientTape() as tape:
                batch_ = random.sample(replay, batch)

                # calculate q-value for current and next states
                q_value_cur = value_network(tf.convert_to_tensor([x[0] for x in batch_]))
                q_value_next = value_network(tf.convert_to_tensor([x[3] for x in batch_]))

                reward = tf.convert_to_tensor([x[2] for x in batch_])
                action = tf.convert_to_tensor([x[1] for x in batch_])
                done = tf.convert_to_tensor([x[4] for x in batch_])

                actual_q_value_cur = tf.cast(reward, tf.float64) + tf.cast(tf.constant(alpha), tf.float64) * (tf.cast(tf.constant(gamma), tf.float64)*tf.cast((tf.constant(1)-done), tf.float64) * tf.cast(tf.reduce_max(q_value_next), tf.float64))

                loss = tf.cast(tf.gather(q_value_cur, action, axis=1, batch_dims=1), tf.float64)
                # print(loss)
                loss = loss - actual_q_value_cur
                loss = tf.reduce_mean(tf.math.pow(loss,2))

                grads = tape.gradient(loss, value_network.trainable_variables)
                optimizer.apply_gradients(zip(grads,value_network.trainable_variables))

                print('Epoch {} done with loss {} !!!!!!'.format(epoch,loss))

                if epoch%100==0:
                    epsilon*=0.999
                    value_network.save(f'practice_results/model_epoch_{epoch}.keras')
                epoch+=1