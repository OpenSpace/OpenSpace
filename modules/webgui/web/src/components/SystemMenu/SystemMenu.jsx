import React, { Component } from 'react';
import Icon from '../common/Icon/Icon';
import Button from '../common/Input/Button';
import styles from './SystemMenu.scss';
import Popover from '../common/Popover/Popover';

class SystemMenu extends Component {
  constructor(props) {
    super(props);
    this.state = { showMenu: false };
    this.toggleMenu = this.toggleMenu.bind(this);
  }

  toggleMenu() {
    this.setState({ showMenu: !this.state.showMenu });
  }

  render() {
    return (
      <div className={styles.SystemMenu}>
        { this.state.showMenu && (
          <Popover className={styles.popover} arrow="arrow bottom leftside">
            <ul className={styles.links}>
              <li>Toggle console <span className="shortcut">~</span></li>
              <hr className={Popover.styles.delimiter} />
              <li>Quit Openspace <span className="shortcut">ESC</span></li>
              <hr className={Popover.styles.delimiter} />
              <li>Help</li>
              <li>Report a problem</li>
              <li>About OpenSpace</li>
            </ul>
          </Popover>
        )}

        <Button className={styles.button} transparent onClick={this.toggleMenu}>
          <Icon icon="more_vert" className={styles.icon} />
        </Button>
      </div>
    );
  }
}

export default SystemMenu;
